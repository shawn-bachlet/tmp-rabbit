
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal.Core where
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, bracket, try)
import Network.AMQP (AMQPException, Channel, openChannel, openConnection')
import Network.Socket (PortNumber)

data Event = Event
  deriving (Eq, Ord)

data StartError = StartError
  deriving Show

instance Exception StartError

type Logger = Event -> IO ()

waitForRabbit :: Int -> IO Channel
waitForRabbit port = do
    -- TODO: add configuration for username/password
  try (openConnection' "127.0.0.01" ((fromInteger . toInteger $ port) :: PortNumber) "/" "guest" "guest")
    >>= \case
      Left (_ :: AMQPException) -> threadDelay 50000 >> waitForRabbit port
      Right conn -> openChannel conn

