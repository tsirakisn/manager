
{-# LANGUAGE OverloadedStrings #-}
module XenMgr.Connect.Xl
    (
    --xl muscly stuff
      start
    , shutdown
    , getDomainId
    , onNotify 
    , onNotifyRemove 
    ) where

import Control.Exception as E
import Control.Applicative
import Control.Monad
import Control.Monad.Error hiding (liftIO)
import Control.Concurrent
import Data.String
import Vm.Types
import System.Cmd
import System.Process
import XenMgr.Rpc
import Tools.Misc
import qualified Data.Map as M

type NotifyHandler = [String] -> Rpc ()
type Params = [(String, String)]

shutdown :: Uuid -> IO ()
shutdown uuid = do
    domid <- getDomainId uuid
    exitCode <- system ("xl shutdown " ++ domid)
    case exitCode of 
      _ -> return ()

start :: Uuid -> IO ()
start uuid = do
    exitCode <- system ("xl create " ++ configPath uuid)
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
        let [uuidV, _, statusV] = signalArgs signal
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
        let [uuidV, _, statusV] = signalArgs signal
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


configPath uuid = "/storage/configs/" ++ show uuid ++ ".cfg"

