main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- TODO: format for example test. Add tests to confirm messages aren't
-- received in separate process exchanges, etc.
-- What needs to be tested?
-- test :: IO ()
-- test =
--   with $ \chan -> do
--     declareQueue chan newQueue {queueName = "myQueue"}
--     declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
--     bindQueue chan "myQueue" "myExchange" "myKey"
--
--     -- subscribe to the queue
--     consumeMsgs chan "myQueue" Ack myCallback
--
--     -- publish a message to our new exchange
--     publishMsg chan "myExchange" "myKey"
--         newMsg {msgBody = BL.pack "hello world",
--                 msgDeliveryMode = Just Persistent}
--
--     putStrLn "enter key press to close connection"
--     getLine -- wait for keypress
--     putStrLn "connection closed"
--
