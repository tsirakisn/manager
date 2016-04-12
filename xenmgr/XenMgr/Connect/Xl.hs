
{-# LANGUAGE OverloadedStrings #-}
module XenMgr.Connect.Xl
    (
    --xl muscly stuff
      start
    , shutdown
    ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.String
import Vm.Types
import System.Cmd
import System.Process
import XenMgr.Rpc

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
getDomainId uuid = 
    readProcess "xl" ["uuid-to-domid", show uuid] []

configPath uuid = "/storage/configs/" ++ show uuid ++ ".cfg"

