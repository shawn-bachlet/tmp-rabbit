{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Internal.Config where

import Control.Applicative (Alternative((<|>)))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try)
import Control.Monad (join, unless, void, when)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Cont (evalContT)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last, getLast)
import Data.Monoid.Generic (GenericMonoid(..), GenericSemigroup(..))
import Data.Text ()
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode)
import GHC.IO.Handle (Handle)
import Internal.Core (Logger, waitForRabbit)
import Network.AMQP (AMQPException, Channel, openChannel, openConnection)
import Network.HostName (getHostName)
import Network.Socket.Free (getFreePort)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.Posix.Signals (sigINT, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process
  ( CreateProcess(..), StdStream(CreatePipe), ProcessHandle, createProcess, proc
  , readProcessWithExitCode, shell, waitForProcess
  )
import System.Process.Internals (ProcessHandle__(OpenHandle), ProcessHandle, withProcessHandle)



data Config = Config
  { rabbitPort         :: Last (Maybe Int)
  , distPort           :: Last (Maybe Int)
  , erlPort            :: Last (Maybe Int)
  , managementPort     :: Last (Maybe Int)
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
  , managementPort                :: Int
  }

newtype ProcessConfig = ProcessConfig {rabbitEnv :: Maybe [(String, String)]}

setupConfig :: Config -> IO Resources
setupConfig config@Config {..} = do
  envs            <- getEnvironment
  tmpEnv          <- lookupEnv "TMP"
  tmpDirEnv       <- lookupEnv "TMPDIR"
  let
    defaultTemp      = fromMaybe "/tmp" $ tmpEnv <|> tmpDirEnv
    resourcesTempDir = fromMaybe defaultTemp $ getLast temporaryDirectory

  resourcesPlan <- Plan resourcesTempDir
      <$> getPort rabbitPort
      <*> getPort erlPort
      <*> getPort distPort
      <*> getPort managementPort

  pure Resources {..}

getPort :: Last (Maybe Int) -> IO Int
getPort port = maybe getFreePort pure $ join $ getLast port

-- TODO: actually implement this
cleanupConfig :: Resources -> IO ()
cleanupConfig = undefined

startPlan :: Plan -> IO (Channel, (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
startPlan plan@Plan{..} = do
  systemEnv <- getEnvironment
  putStrLn $ "Starting on port " <> show rabbitPort
  putStrLn $ "rabbitmq tmp folder is: " <> show completePlanDataDirectory
  nodeName <- nextRandom
  hostName <- getHostName

  disablePlugins
  handles <- createProcess (proc "rabbitmq-server" []){
      env = buildEnvironment plan systemEnv nodeName hostName
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

-- TODO: if we keep this, change the below comment to something better.
-- | rabbitmq_mqtt and rabbitmq_stomp are disabled to poorly handle a port conflict.
disablePlugins :: IO ()
disablePlugins = do
  (_, _, _, pluginHandle) <- createProcess (shell "rabbitmq-plugins disable rabbitmq_mqtt rabbitmq_stomp")
  void $ waitForProcess pluginHandle

buildEnvironment :: Plan -> [(String, String)] -> UUID -> String -> Maybe [(String, String)]
buildEnvironment plan@Plan{..} systemEnv nodeName hostName =
  Just $ ("RABBITMQ_LOGS", "-")
       : ("USE_LONGNAME", "true")
       : ("RABBITMQ_NODE_PORT", show rabbitPort)
       : ("RABBITMQ_DIST_PORT", show distPort)
       : ("RABBITMQ_NODENAME", show nodeName <> "@" <> hostName)
       : ("RABBITMQ_SERVER_START_ARGS", "-rabbitmq_management listener [{port," <> show managementPort <> "}]")
       : ("ERL_EPMD_PORT", show erlPort)
       : systemEnv
