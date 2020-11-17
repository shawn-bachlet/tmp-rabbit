{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Internal.Config where

import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.Trans ( MonadTrans(lift) )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( getLast, Last )
import Data.Monoid.Generic
    ( GenericMonoid(..), GenericSemigroup(..) )
import Data.UUID.V4 (nextRandom)
import Internal.Core ( Logger, waitForRabbit )
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try)
import Control.Monad (unless, when, join)
import Control.Monad.Trans.Cont (evalContT)
import Data.Text ()
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode)
import GHC.IO.Handle (Handle)
import Network.AMQP (AMQPException, Channel, openChannel, openConnection)
import Network.HostName (getHostName)
import Network.Socket.Free ( getFreePort )
import System.Environment ( getEnvironment, lookupEnv )
import System.Exit (ExitCode(ExitSuccess))
import System.Posix.Signals (sigINT, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process ( StdStream( CreatePipe ), CreateProcess(..), ProcessHandle, createProcess, proc, readProcessWithExitCode, waitForProcess)
import System.Process.Internals (ProcessHandle__(OpenHandle), ProcessHandle, withProcessHandle)



data Config = Config
  { rabbitPort         :: Last (Maybe Int)
  , distPort           :: Last (Maybe Int)
  , erlPort            :: Last (Maybe Int)
  , temporaryDirectory :: Last FilePath
  }
  deriving           stock (Generic)
  deriving Semigroup via   GenericSemigroup Config
  deriving Monoid    via   GenericMonoid    Config

data Resources = Resources
  { resourcesPlan    :: Plan
  , resourcesTempDir :: FilePath
  }

data Plan = Plan
  { completePlanDataDirectory     :: FilePath
  , rabbitPort                    :: Int
  , distPort                      :: Int
  , erlPort                       :: Int
  }

newtype ProcessConfig = ProcessConfig {rabbitEnv :: Maybe [(String, String)]}

setupConfig :: Config -> IO Resources
setupConfig config@Config {..} = evalContT $ do
  envs      <- lift getEnvironment
  openPort  <- lift $ maybe getFreePort pure $ join $ getLast rabbitPort
  secondPort <- lift $ maybe getFreePort pure $ join $ getLast erlPort
  anotherPort <- lift $ maybe getFreePort pure $ join $ getLast distPort
  tmpEnv    <- lift $ lookupEnv "TMP"
  tmpDirEnv <- lift $ lookupEnv "TMPDIR"
  let
    defaultTemp      = fromMaybe "/tmp" $ tmpEnv <|> tmpDirEnv
    resourcesTempDir = fromMaybe defaultTemp $ getLast temporaryDirectory
    resourcesPlan = Plan resourcesTempDir openPort anotherPort secondPort
  pure Resources {..}

cleanupConfig :: Resources -> IO ()
cleanupConfig = undefined

startPlan :: Plan -> IO (Channel, (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
startPlan plan@Plan{..} = do
  systemEnv <- getEnvironment
  putStrLn $ "Starting on port " <> show rabbitPort
  putStrLn $ "rabbitmq tmp folder is: " <> show completePlanDataDirectory
  randomNumber <- nextRandom
  hostName <- getHostName
  handles <- createProcess (proc "rabbitmq-server" []){
      env = Just $ ("RABBITMQ_LOGS", "-")
                 : ("USE_LONGNAME", "true")
                 -- : ("RABBITMQ_MNESIA_DIR", completePlanDataDirectory)
                 : ("RABBITMQ_NODE_PORT", show rabbitPort)
                 : ("RABBITMQ_DIST_PORT", show distPort)
                 : ("RABBITMQ_NODENAME", (show randomNumber <> "@" <> hostName))
                 -- : ("ERL_EPMD_PORT", show erlPort)
                 : systemEnv
    }
  chan <- waitForRabbit rabbitPort
  putStrLn "Started Rabbit..."
  pure (chan, handles)

stopPlan :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ExitCode
stopPlan (_, _, _, processHandle) = do
  withProcessHandle processHandle $ \case
    OpenHandle p -> signalProcess sigINT p
    _             -> pure ()
  waitForProcess processHandle
