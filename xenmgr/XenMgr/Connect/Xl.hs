
{-# LANGUAGE OverloadedStrings #-}
module XenMgr.Connect.Xl
    (
    --xl muscly stuff
      start
    , domainID
    , domainXsPath
    , shutdown
    , getDomainId
    , unpause
    , destroy 
    , onNotify 
    , onNotifyRemove
    , isRunning
    , isFocused
    , state
    , resumeFromSleep
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
import System.Cmd
import System.Process
import XenMgr.Rpc
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

shutdown :: Uuid -> IO ()
shutdown uuid = do
    domid <- getDomainId uuid
    exitCode <- system ("xl shutdown " ++ domid)
    case exitCode of 
      _ -> return ()

unpause :: Uuid -> IO ()
unpause uuid = do
    domid <- getDomainId uuid
    exitCode <- system ("xl unpause " ++ domid)
    case exitCode of
      _ -> return ()

start :: Uuid -> IO ()
start uuid = do
    exitCode <- system ("xl create " ++ configPath uuid ++ " -p")
    case exitCode of
      _ -> return ()

destroy :: Uuid -> IO ()
destroy uuid = do 
    domid <- getDomainId uuid
    exitCode <- system ("xl destroy " ++ domid)
    case exitCode of
      _ -> return ()

getDomainId :: Uuid -> IO String
getDomainId uuid = do 
    domid <- readProcess "xl" ["uuid-to-domid", show uuid] []
    case domid of 
      "-1" -> return ("")
      _    -> return (domid)


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
