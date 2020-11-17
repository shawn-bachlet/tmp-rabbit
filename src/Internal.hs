{-# LANGUAGE RecordWildCards #-}

module Internal where

import Control.Exception (bracketOnError, bracket, try)
import Control.Monad.Trans.Cont (evalContT, ContT(..))
import Internal.Config (Plan(..), Config, Resources(..), setupConfig, cleanupConfig, startPlan, stopPlan)
import Internal.Core (StartError)
import GHC.IO.Handle (Handle)
import System.Process.Internals (ProcessHandle__(OpenHandle), ProcessHandle, withProcessHandle)
import Network.AMQP (AMQPException, Channel, openChannel, openConnection)

startConfig :: Config
          -- ^ @extra@ configuration that is 'mappend'ed last to the generated `Config`.
          -- @generated@ '<>' @extra@.
          -> IO (Channel, (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
startConfig extra = do
  Resources plan _ <- setupConfig extra 
  startPlan plan 
data Rabbit = Rabbit