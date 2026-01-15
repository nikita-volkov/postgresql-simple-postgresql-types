module Main.Helpers where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Proxy
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable
import Data.Word
import qualified Database.PostgreSQL.Simple as Ps
import qualified Database.PostgreSQL.Simple.FromField as Ps
import qualified Database.PostgreSQL.Simple.PostgresqlTypes ()
import qualified Database.PostgreSQL.Simple.ToField as Ps
import qualified Database.PostgreSQL.Simple.Types as Ps
import qualified PostgresqlTypes as PostgresqlTypes
import Test.Hspec
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QuickCheck
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
  withPool poolSize Ps.connect Ps.close

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

-- | Test roundtrip encoding/decoding via postgresql-simple
mappingSpec ::
  forall a.
  ( HasCallStack,
    QuickCheck.Arbitrary a,
    Show a,
    Eq a,
    PostgresqlTypes.IsStandardType a,
    Ps.ToField a,
    Ps.FromField a,
    Typeable a
  ) =>
  Proxy a ->
  SpecWith Ps.Connection
mappingSpec _ =
  let typeName = untag (PostgresqlTypes.typeName @a)
   in describe "Roundtrip" do
        describe "Single value roundtrip" do
          it "Should encode and decode to the same value" \(connection :: Ps.Connection) ->
            QuickCheck.property \(value :: a) -> do
              QuickCheck.idempotentIOProperty do
                -- Use postgresql-simple to roundtrip the value with explicit type casting
                results <-
                  Ps.query
                    connection
                    ( Ps.Query
                        ( Text.encodeUtf8
                            ("SELECT (?::" <> typeName <> ")")
                        )
                    )
                    (Ps.Only value)
                case results of
                  [Ps.Only (decoded :: a)] -> do
                    pure (decoded === value)
                  _ -> do
                    fail $ "Expected exactly one result, got: " <> show (length results)

        describe "Array roundtrip" do
          it "Should encode and decode arrays correctly" \(connection :: Ps.Connection) ->
            QuickCheck.property \(values :: [a]) -> do
              QuickCheck.idempotentIOProperty do
                -- Use postgresql-simple to roundtrip array values with explicit type casting
                results <-
                  Ps.query
                    connection
                    ( Ps.Query
                        ( Text.encodeUtf8
                            ("SELECT (?::" <> typeName <> "[])")
                        )
                    )
                    (Ps.Only (Ps.PGArray values))
                case results of
                  [Ps.Only (Ps.PGArray (decoded :: [a]))] -> do
                    pure (decoded === values)
                  _ -> do
                    fail $ "Expected exactly one array result, got: " <> show (length results)

        describe "NULL handling" do
          it "Should decode NULL values appropriately" \(connection :: Ps.Connection) -> do
            results <- Ps.query_ connection "SELECT NULL"
            case results of
              [Ps.Only (maybeValue :: Maybe a)] -> do
                maybeValue `shouldBe` Nothing
              _ -> do
                fail $ "Expected exactly one result with NULL, got: " <> show (length results)
