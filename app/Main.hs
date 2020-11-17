{-# LANGUAGE OverloadedStrings #-}

module Main where

import Internal

import Network.AMQP

import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  -- TODO: this an example that shows a message being passed from a queue
  -- in a single exchange. This could be a nice test. We want to
  -- simply start rabbitMQ here.
  with $ \chan -> do
    declareQueue chan newQueue {queueName = "myQueue"}
    declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
    bindQueue chan "myQueue" "myExchange" "myKey"

    -- subscribe to the queue
    consumeMsgs chan "myQueue" Ack myCallback

    -- publish a message to our new exchange
    publishMsg chan "myExchange" "myKey"
        newMsg {msgBody = BL.pack "hello world",
                msgDeliveryMode = Just Persistent}

    putStrLn "enter key press to close connection"
    getLine -- wait for keypress
    putStrLn "connection closed"

myCallback :: (Message,Envelope) -> IO ()
myCallback (msg, env) = do
    putStrLn $ "received message: " ++ BL.unpack (msgBody msg)
    -- acknowledge receiving the message
    ackEnv env



