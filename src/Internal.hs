module Internal where
import Control.Exception (bracket, bracketOnError, try)
import Control.Monad.Trans.Cont (ContT(..), evalContT)
import GHC.IO.Handle (Handle)
import Internal.Config (Plan(..), Resources(..), Config, cleanupConfig, setupConfig, startPlan, stopPlan)
import Internal.Core (StartError)
import Network.AMQP (AMQPException, Channel, openChannel, openConnection)
import System.Process.Internals (ProcessHandle__(OpenHandle), ProcessHandle, withProcessHandle)

startConfig :: Config
          -- ^ @extra@ configuration that is 'mappend'ed last to the generated `Config`.
          -- @generated@ '<>' @extra@.
          -> IO (Channel, (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
startConfig extra = do
  Resources plan _ <- setupConfig extra 
  startPlan plan 
data Rabbit = Rabbit
