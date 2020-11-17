{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try)
import Control.Monad (unless, when)
import Control.Monad.Trans.Cont (evalContT)
import Data.Monoid (Last(..), getLast)
import Data.Text ()
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode)
import GHC.IO.Handle (Handle)
import Internal (startConfig)
import Internal.Config (Config(..), Plan(..), ProcessConfig(..), setupConfig, startPlan, stopPlan)
import Network.AMQP (AMQPException, Channel, openChannel, openConnection)
import System.Exit (ExitCode(ExitSuccess))
import System.Posix.Signals (sigINT, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess(..), ProcessHandle, createProcess, proc, readProcessWithExitCode, waitForProcess)
import System.Process.Internals (ProcessHandle__(OpenHandle), ProcessHandle, withProcessHandle)

withConfig :: [String] -> (Channel -> IO a) -> IO a
withConfig config action = do
  putStrLn "Starting Rabbit ..."
--  setupConfig config
  bracket (startConfig $ Config (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)) (stopPlan . snd) (action . fst)

with :: (Channel -> IO a) -> IO a
with = withConfig []

ensureRabbitMQInstalled :: RabbitMQConfig -> IO ()
ensureRabbitMQInstalled config = do
  detectExeInstalled (rabbitExe config) "rabbitmq-server"

detectExeInstalled :: FilePath -> FilePath -> IO ()
detectExeInstalled configuredExe defaultExe = do
  when (configuredExe == defaultExe) $ do
    x <- exeExists defaultExe
    unless x $ error $
      "Looks like " <> defaultExe <> " is either not installed or not on your"
        <> " PATH; it can be installed with 'brew install rabbitmq' and then"
        <> " including it in your PATH with"
        <> " 'export PATH=$PATH:/usr/local/sbin'"

exeExists :: FilePath -> IO Bool
exeExists exe =
  flip fmap (readProcessWithExitCode "command" ["-v", exe] "") $ \case
    (ExitSuccess, _, _) -> True
    _ -> False

data RabbitMQConfig = RabbitMQConfig
  { enabled :: Bool
  , rabbitExe :: String
  } deriving stock (Eq, Generic, Show)
