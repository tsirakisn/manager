
{-# LANGUAGE OverloadedStrings #-}
module XenMgr.Connect.Xl
    (
    --xl domain control
      start
    , shutdown
    , unpause
    , pause
    , destroy 
    , resumeFromSleep
    , reboot
    , sleep
    --,hibernate  (Xl doesn't support s4 at the moment?)
    , suspendToFile
    , resumeFromFile 
    , changeCd
    , setMemTarget
    , acpiState
    , waitForAcpiState

    --xl/toolstack queries
    , domainID
    , domainXsPath
    , getDomainId
    , isRunning
    , isFocused
    , state

    --dbus stuff
    , onNotify 
    , onNotifyRemove
    , xlSurfmanDbus 
    , xlInputDbus
    ) where

import Control.Exception as E
import Control.Applicative
import Control.Monad
import Control.Monad.Error hiding (liftIO)
import Control.Concurrent
import Data.String
import Vm.Types
import Vm.State
import Tools.Misc
import Tools.XenStore
import Tools.Log
import System
import System.Cmd
import System.Process
import System.Directory
import XenMgr.Rpc
import XenMgr.Db
import qualified Data.Map as M

type NotifyHandler = [String] -> Rpc ()
type Params = [(String, String)]


--wiredMac :: Uuid -> Rpc (Maybe String)
--wiredMac uuid = wiredNic uuid >>- return . maybe Nothing $ Just . nicMac)
--
--wiredNic :: Uuid -> Rpc (Maybe Nic)
--wiredNic uuid = 
--    do ns <- nics uuid
--        case ns of 
--          []   -> return Nothing
--          n: _ -> return $ Just n 

--nics :: Uuid -> Rpc [Nic]
--nics uuid = 
  
resumeFromSleep :: Uuid -> IO ()
resumeFromSleep uuid = do
    domid <- getDomainId uuid
    exitCode <- system ("xl trigger " ++ domid ++ " s3resume") 
    case exitCode of
      _  -> return ()
 
domainID :: Uuid -> IO (Maybe DomainID)
domainID uuid = do
    domid  <- getDomainId uuid
    return $ if domid == "" then Nothing else Just (read domid :: DomainID)

waitForAcpiState :: Uuid -> Int -> Maybe Int -> IO Bool
waitForAcpiState uuid expected timeout = do
    s <- acpiState uuid
    case  (s, timeout) of
        (x, _)          | x == expected -> return True
        (_, Just t)     | t <= 0        -> return False
        (_, Just t)     | t > 0         -> do liftIO (threadDelay $ 10^6)
                                              waitForAcpiState uuid expected (Just $ t-1)
        (_, Nothing)                    -> liftIO (threadDelay $ 10^6) >> waitForAcpiState uuid expected Nothing
        _                               -> error "impossible"

acpiState :: Uuid -> IO AcpiState
acpiState uuid = do 
    domid    <- getDomainId uuid
    acpi_state <- readProcess "xl" ["acpi-state", show domid] [] 
    return $ (read acpi_state :: Int)

isFocused :: Uuid -> IO Bool
isFocused uuid = do
    s <- state uuid
    p <- domainXsPath uuid
    haveFocus s p
  where
    
    haveFocus Shutdown _    = return False
    haveFocus _        domP = let path = domP ++ "/switcher/have_focus" in
                              liftIO $ xsRead path >>= return . maybe False (== "1")

domainXsPath :: Uuid -> IO String
domainXsPath uuid = do
    domid <- getDomainId uuid
    case domid of
      "" -> return $ "/local/domain/unknown"
      _  -> return $ "/local/domain/" ++ show domid 

reboot :: Uuid -> IO ()
reboot uuid = 
    do
      domid <- getDomainId uuid
      exitCode <- system ("xl reboot " ++ domid)
      case exitCode of
        ExitSuccess   -> return ()
        _             -> do _ <- system ("xl reboot -F " ++ domid)
                            return ()

shutdown :: Uuid -> IO ()
shutdown uuid = 
    do
      domid <- getDomainId uuid
      exitCode  <- system ("xl shutdown " ++ domid)
      case exitCode of
        ExitSuccess   -> return ()
        _             -> do _ <- system ("xl shutdown -F " ++ domid)
                            return ()

pause :: Uuid -> IO ()
pause uuid = 
    do
      domid <- getDomainId uuid
      _     <- system ("xl pause " ++ domid)
      return ()

unpause :: Uuid -> IO ()
unpause uuid = do
    domid <- getDomainId uuid
    exitCode <- system ("xl unpause " ++ domid)
    case exitCode of
      _ -> return ()

start :: Uuid -> IO ()
start uuid = 
    do
        state <- state uuid
        case state of
            Shutdown -> do _  <- system ("xl create " ++ configPath uuid ++ " -p")
                           return ()
            _ -> do return ()

destroy :: Uuid -> IO ()
destroy uuid = do 
    domid <- getDomainId uuid
    exitCode <- system ("xl destroy " ++ domid)
    case exitCode of
      _ -> return ()

sleep :: Uuid -> IO ()
sleep uuid = 
    do
      domid <- getDomainId uuid
      _     <- system ("xl trigger " ++ domid ++ " sleep")
      return ()

suspendToFile :: Uuid -> FilePath -> IO ()
suspendToFile uuid file = 
    do
      domid <- getDomainId uuid
      _     <- system ("xl save " ++ domid ++ " " ++ file)
      return ()

resumeFromFile :: Uuid -> FilePath -> Bool -> Bool -> IO ()
resumeFromFile uuid file delete paused = 
    do
      let p = if paused then "-p" else ""
      _ <- system ("xl restore " ++ p ++ file)
      if delete then removeFile file else return ()

getDomainId :: Uuid -> IO String
getDomainId uuid = do 
    domid <- readProcess "xl" ["uuid-to-domid", show uuid] []
    case domid of 
      "-1" -> return ("")
      _    -> return (domid)

changeCd :: Uuid -> String -> IO ()
changeCd uuid path = do
    domid <- getDomainId uuid
    _     <- system ("xl cd-insert " ++ domid ++ " hdc " ++ path)
    return ()

nicFrontendPath :: Uuid -> NicID -> IO (Maybe String)
nicFrontendPath uuid (XbDeviceID nicid) =
    do domainP <- domainXsPath uuid
       vifs   <- liftIO . xsDir $ domainP ++ "/device/vif"
       vwifs  <- liftIO . xsDir $ domainP ++ "/device/vwif"
       let nicid_str = show nicid
       case () of
         _ | nicid_str `elem` vifs -> return $ Just (domainP ++ "/device/vif/" ++ nicid_str)
           | nicid_str `elem` vwifs -> return $ Just (domainP ++ "/device/vwif" ++ nicid_str)
           | otherwise -> return Nothing


connectVif :: Uuid -> NicID -> Bool -> IO ()
connectVif uuid nicid connect = do
    domainP <- domainXsPath uuid
    front   <- nicFrontendPath uuid nicid
    case front of
        Nothing -> warn $ "failed to lookup nic " ++ show nicid
        Just fp -> do let p = fp ++ "/disconnect"
                      liftIO $ xsWrite p value
  where
    value | connect == True         = "0"
          | otherwise               = "1"

--Adjust memory through the balloon driver, unreliable, requires correct
--paravirt drivers.
setMemTarget :: Uuid -> Int -> IO ()
setMemTarget uuid mbs = do
    domid <- getDomainId uuid
    _     <- system ("xl mem-set " ++ domid ++ " " ++ show mbs ++ "m")
    return ()

onNotify :: Uuid -> String -> NotifyHandler -> Rpc ()
onNotify uuid msgname action =
    let rule = matchSignal "com.citrix.xenclient.xenmgr" "notify"
    in
      rpcOnSignal rule process
  where
    process _ signal =
        let [uuidV, statusV] = signalArgs signal
            uuid'  = let Just v = fromVariant $ uuidV in v
            status = let Just v = fromVariant $ statusV in v
            splits = split ':' status
        in
          when (uuid == uuid') $
               case splits of
                 (msg:args) | msg == msgname    -> action args
                 _                              -> return ()

onNotifyRemove :: Uuid -> String -> NotifyHandler -> Rpc ()
onNotifyRemove uuid msgname action =
    let rule = matchSignal "com.citrix.xenclient.xenmgr" "notify"
    in
        rpcOnSignalRemove rule process
  where
    process _ signal =
        let [uuidV, statusV] = signalArgs signal
            uuid'  = let Just v = fromVariant $ uuidV in v
            status = let Just v = fromVariant $ statusV in v
            splits = split ':' status
        in
          when (uuid == uuid') $
               case splits of
                 (msg:args) | msg == msgname    -> action args
                 _                              -> return ()


invoke :: MonadRpc e m => Uuid -> String -> Params -> m [Variant]
invoke uuid method params =
    rpcXl uuid method [toVariant $ pack params]

pack :: Params -> M.Map String String
pack = M.fromList

rpcXl :: MonadRpc e m => Uuid -> String -> [Variant] -> m [Variant]
rpcXl uuid method = rpcCallOnce . xlcall uuid method

xlcall :: Uuid -> String -> [Variant] -> RpcCall
xlcall uuid memb args =
  RpcCall service object interface (fromString memb) args
  where
    service = fromString $ "xl.signal.notify"
    interface = fromString $ "xl.signal.notify"
    object = fromString $ "/xl/signal/notify"

xlSurfmanDbus uuid memb args = 
  RpcCall service object interface (fromString memb) args
  where
    service = fromString $ "com.citrix.xenclient.surfman"
    interface = fromString $ "com.citrix.xenclient.surfman"
    object = fromString $ "/"

xlInputDbus uuid memb args = 
  RpcCall service object interface (fromString memb) args
  where
    service = fromString $ "com.citrix.xenclient.input"
    interface = fromString $ "com.citrix.xenclient.input"
    object = fromString $ "/"

configPath uuid = "/tmp/xenmgr-xl-" ++ show uuid
stubConfigPath uuid = "/tmp/xenmgr-xl-" ++ show uuid ++ "-dm"

isRunning :: (MonadRpc e m) => Uuid -> m Bool
isRunning uuid = (not . (`elem` [Shutdown, Rebooted])) `fmap` (liftIO $ state uuid)

state :: Uuid -> IO VmState
state uuid = 
    do
        maybe_state <- xsRead ("/state/" ++ show uuid ++ "/state")
        case maybe_state of
          Just state -> do 
                          info $ "active vms, state = " ++ show state
                          return $ stateFromStr state
          Nothing    -> return $ stateFromStr "shutdown"


waitForState :: Uuid -> VmState -> Maybe Int -> IO Bool
waitForState uuid expected timeout = do
    s <- state uuid
    case (s, timeout) of
      -- got right state, exit
      (x, _)       | x == expected -> return True

      -- we timed out while waiting for state
      (_, Just t)  | t <= 0        -> return False

      -- we continue waiting with lesser timeout
      (_, Just t)  | t >  0        -> do (threadDelay $ 10^6)
                                         waitForState uuid expected (Just $ t-1)

      -- we have no timeout, wait indifinitely
      (_, Nothing)                 -> (threadDelay $ 10^6) >> waitForState uuid expected Nothing
      _                            -> error "impossible"
