
{-# LANGUAGE OverloadedStrings #-}
module XenMgr.Connect.Xl
    (
    --xl muscly stuff
      start
    , shutdown
    , destroy
    ) where

import Control.Applicative
import Data.String
import System.Command

shutdown :: Uuid -> IO ()
shutdown uuid = do
    exitCode <- system "xl shutdown " ++ liftIO $ getDomainId uuid
    return ()

start :: Uuid -> IO ()
start uuid = do
    exitCode <- system "xl create " ++ configPath uuid 
    return ()

getDomainId :: Uuid -> IO String
getDomainId uuid = 
    readProcess "xl" ["domid", uuid] []

configPath uuid = "/storage/configs/" ++ uuid ++ ".cfg"

