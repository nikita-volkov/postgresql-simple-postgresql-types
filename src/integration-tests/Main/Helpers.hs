module Main.Helpers where

import Control.Exception
import Control.Monad
import Data.Proxy
import Data.String
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import Data.Word
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.PostgresqlTypes ()
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified PostgresqlTypes as PostgresqlTypes
import Test.Hspec
import Test.QuickCheck ((.&&.), (===))
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
withConnection :: SpecWith PG.Connection -> SpecWith (Text, Word16)
withConnection =
  describe "postgresql-simple connection" . aroundWith acquireConnection
  where
    acquireConnection actionWithConnection (host, port) =
      bracket
        (PG.connect connectionInfo)
        PG.close
        actionWithConnection
      where
        connectionInfo =
          PG.defaultConnectInfo
            { PG.connectHost = Text.unpack host,
              PG.connectPort = fromIntegral port,
              PG.connectUser = "postgres",
              PG.connectPassword = "postgres",
              PG.connectDatabase = "postgres"
            }

-- | Helper to describe a test for a specific type
withType :: forall a b. (Typeable a) => [Proxy a -> SpecWith b] -> SpecWith b
withType specs = do
  describe (show (typeOf (Proxy @a))) do
    mapM_ (\spec -> spec Proxy) specs

-- | Test roundtrip encoding/decoding via postgresql-simple
mappingSpec ::
  forall a.
  ( HasCallStack,
    QuickCheck.Arbitrary a,
    Show a,
    Eq a,
    PostgresqlTypes.IsStandardType a,
    PG.ToField a,
    PG.FromField a,
    Typeable a
  ) =>
  Proxy a ->
  SpecWith PG.Connection
mappingSpec _ =
  let typeName = untag (PostgresqlTypes.typeName @a)
      maybeBaseOid = untag (PostgresqlTypes.baseOid @a)
      maybeArrayOid = untag (PostgresqlTypes.arrayOid @a)
   in describe "Roundtrip" do
        describe (Text.unpack typeName) do
          describe "Single value roundtrip" do
            it "Should encode and decode to the same value" \(connection :: PG.Connection) ->
              QuickCheck.property \(value :: a) -> do
                QuickCheck.idempotentIOProperty do
                  -- Use postgresql-simple to roundtrip the value
                  -- SELECT ? sends the value and gets it back, testing both ToField and FromField
                  results <- PG.query connection "SELECT ?" (PG.Only value)
                  case results of
                    [PG.Only (decoded :: a)] -> do
                      pure (decoded === value)
                    _ -> do
                      fail $ "Expected exactly one result, got: " <> show (length results)

          describe "Array roundtrip" do
            it "Should encode and decode arrays correctly" \(connection :: PG.Connection) ->
              QuickCheck.property \(values :: [a]) -> do
                QuickCheck.idempotentIOProperty do
                  -- Use postgresql-simple to roundtrip array values
                  results <- PG.query connection "SELECT ?" (PG.Only (PG.PGArray values))
                  case results of
                    [PG.Only (PG.PGArray (decoded :: [a]))] -> do
                      pure (decoded === values)
                    _ -> do
                      fail $ "Expected exactly one array result, got: " <> show (length results)

          describe "NULL handling" do
            it "Should handle NULL values appropriately" \(connection :: PG.Connection) -> do
              results <- PG.query connection "SELECT NULL" PG.Only
              case results of
                [PG.Only (maybeValue :: Maybe a)] -> do
                  maybeValue `shouldBe` Nothing
                _ -> do
                  fail $ "Expected exactly one result with NULL, got: " <> show (length results)

          describe "OID validation" do
            it "Should validate base and array OIDs" \(connection :: PG.Connection) -> do
              -- Query the database for OID information
              let oidQuery = "SELECT oid, typarray FROM pg_type WHERE typname = ?"
              oidResults <- PG.query connection oidQuery (PG.Only typeName) :: IO [(Word32, Word32)]

              case oidResults of
                [(actualBaseOid, actualArrayOid)] -> do
                  case maybeBaseOid of
                    Just expectedBaseOid -> actualBaseOid `shouldBe` expectedBaseOid
                    Nothing -> actualBaseOid `shouldSatisfy` (> 0) -- Just verify OID exists
                  case maybeArrayOid of
                    Just expectedArrayOid -> actualArrayOid `shouldBe` expectedArrayOid
                    Nothing -> actualArrayOid `shouldSatisfy` (> 0) -- Just verify OID exists
                _ -> do
                  -- For types not in pg_type, skip this check
                  pure ()

-- | Special test for hstore to verify it works without stable OIDs
hstoreMetadataSpec ::
  forall a.
  ( HasCallStack,
    PostgresqlTypes.IsStandardType a,
    PG.ToField a,
    PG.FromField a,
    Typeable a
  ) =>
  Proxy a ->
  SpecWith PG.Connection
hstoreMetadataSpec _ =
  let typeName = untag (PostgresqlTypes.typeName @a)
      maybeBaseOid = untag (PostgresqlTypes.baseOid @a)
   in describe "Hstore metadata" do
        it "Should work without stable OIDs" \(connection :: PG.Connection) -> do
          -- Hstore and similar extension types don't have stable OIDs
          case maybeBaseOid of
            Nothing -> do
              -- Verify that the type exists in the database
              let typeQuery = "SELECT EXISTS(SELECT 1 FROM pg_type WHERE typname = ?)"
              [PG.Only (exists :: Bool)] <- PG.query connection typeQuery (PG.Only typeName)
              exists `shouldBe` True
            Just _ -> do
              -- If it has a stable OID, that's fine too
              pure ()
