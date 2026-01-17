module IntegrationTests.Scopes where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import Data.Word
import qualified Database.PostgreSQL.Simple as Ps
import qualified Database.PostgreSQL.Simple.PostgresqlTypes ()
import qualified Database.PostgreSQL.Simple.Types as Ps
import Test.Hspec
import qualified TestcontainersPostgresql
import Prelude

-- | Run specs with a PostgreSQL container
withContainer :: Text -> SpecWith (Text, Word16) -> Spec
withContainer tagName =
  describe (Text.unpack tagName) . aroundAll (TestcontainersPostgresql.run config)
  where
    config =
      TestcontainersPostgresql.Config
        { forwardLogs = False,
          auth = TestcontainersPostgresql.TrustAuth,
          tagName
        }

-- | Create a connection from container info
withConnection :: SpecWith Ps.Connection -> SpecWith (Text, Word16)
withConnection =
  withConnectInfo . withConnectionPool 100 . withTQueueElement pure

withConnectInfo :: SpecWith Ps.ConnectInfo -> SpecWith (Text, Word16)
withConnectInfo =
  mapSubject \(host, port) ->
    Ps.defaultConnectInfo
      { Ps.connectHost = Text.unpack host,
        Ps.connectPort = fromIntegral @Word16 port,
        Ps.connectUser = "postgres",
        Ps.connectPassword = "postgres",
        Ps.connectDatabase = "postgres"
      }

withConnectionPool :: Int -> SpecWith (TQueue Ps.Connection) -> SpecWith Ps.ConnectInfo
withConnectionPool poolSize =
  withPool poolSize acquire Ps.close
  where
    acquire connectInfo = do
      connection <- Ps.connect connectInfo
      void do
        Ps.execute_ connection "SET client_min_messages TO WARNING"
      createExtensionIfNotExists connection "hstore"
      pure connection
      where
        createExtensionIfNotExists connection extension =
          handle handler do
            void do
              Ps.execute connection "CREATE EXTENSION IF NOT EXISTS ?" (Ps.Only (Ps.Identifier extension))
          where
            handler (e :: Ps.SqlError) =
              if Ps.sqlState e == "23505"
                && Ps.sqlErrorMsg e == "duplicate key value violates unique constraint \"pg_extension_name_index\""
                then pure ()
                else throwIO e

withPool ::
  -- | Pool size.
  Int ->
  -- | Acquire.
  (b -> IO a) ->
  -- | Release.
  (a -> IO ()) ->
  SpecWith (TQueue a) ->
  SpecWith b
withPool poolSize acquire release =
  aroundAllWith \actionWithQueue b ->
    bracket
      ( do
          queue <- newTQueueIO
          replicateConcurrently_ poolSize do
            element <- acquire b
            atomically $ writeTQueue queue element
          pure queue
      )
      ( \queue -> do
          replicateConcurrently_ poolSize do
            element <- atomically $ readTQueue queue
            release element
      )
      actionWithQueue

withTQueueElement ::
  -- | Clean. Called upon returning to the pool.
  (a -> IO a) ->
  SpecWith a ->
  SpecWith (TQueue a)
withTQueueElement clean =
  aroundWith \actionWithElement queue ->
    bracket
      (atomically $ readTQueue queue)
      ( \element -> do
          element <- clean element
          atomically $ writeTQueue queue element
      )
      actionWithElement

-- | Helper to describe a test for a specific type
withType :: forall a b. (Typeable a) => [Proxy a -> SpecWith b] -> SpecWith b
withType specs = do
  describe (show (typeOf (undefined :: a))) do
    mapM_ (\spec -> spec Proxy) specs
