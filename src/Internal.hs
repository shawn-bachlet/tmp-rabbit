{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Internal where

import Control.Exception (bracket, bracketOnError, try)
import Control.Monad.Trans.Cont (ContT(..), evalContT)
import Data.Monoid (Last(..), getLast)
import GHC.IO.Handle (Handle)
import Internal.Config (Plan(..), Resources(..), Config(..), cleanupConfig, setupConfig, startPlan, stopPlan)
import Internal.Core (StartError)
import Network.AMQP (AMQPException, Channel, openChannel, openConnection)
import System.Process.Internals (ProcessHandle__(OpenHandle), ProcessHandle, withProcessHandle)

startConfig :: Config ->
  IO (Channel, (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
startConfig extra = do
  Resources plan _ <- setupConfig extra
  startPlan plan

withConfig :: [String] -> (Channel -> IO a) -> IO a
withConfig config action = do
  putStrLn "Starting Rabbit ..."
--  setupConfig config
  bracket (startConfig $ Config (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)) (stopPlan . snd) (action . fst)

with :: (Channel -> IO a) -> IO a
with = withConfig []
