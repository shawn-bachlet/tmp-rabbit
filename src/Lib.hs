{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Text
import GHC.IO.Handle (Handle)
import GHC.IO.Exception (ExitCode)
import Network.AMQP
import System.Process
import System.Process.Internals 
import System.Posix.Signals (sigINT, signalProcess)
import System.Posix.Types (ProcessID)

waitForRabbit :: IO Channel
waitForRabbit = do
  try (openConnection "127.0.0.1" "/" "guest" "guest")
    >>= \case
      Left (_ :: AMQPException) -> threadDelay 5000 >> waitForRabbit
      Right conn -> openChannel conn 

withConfig :: Config -> (Channel -> IO a) -> IO a 
withConfig config action = do
  putStrLn "Starting Rabbit ..."
  bracket (startRabbit config) (stopRabbit . snd) (action . fst)

with :: (Channel -> IO a) -> IO a 
with = withConfig defaultConfig

newtype Config = Config {unConfig :: [String]}
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config []

startRabbit :: Config -> IO (Channel, (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
startRabbit config = do
  handles <- createProcess . proc "rabbitmq-server" . unConfig $ config
  chan <- waitForRabbit
  pure (chan, handles)


stopRabbit :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ExitCode 
stopRabbit (_, _, _, processHandle) = do
  withProcessHandle processHandle $ \case 
    OpenHandle p -> signalProcess sigINT p 
    _             -> pure () 
  waitForProcess processHandle
