module IntegrationTests.Scripts where

import Control.Monad
import Data.Proxy
import Data.Tagged
import qualified Data.Text.Encoding as Text
import Data.Typeable
import qualified Database.PostgreSQL.Simple as Ps
import qualified Database.PostgreSQL.Simple.FromField as Ps
import qualified Database.PostgreSQL.Simple.PostgresqlTypes ()
import qualified Database.PostgreSQL.Simple.ToField as Ps
import qualified Database.PostgreSQL.Simple.Types as Ps
import qualified PostgresqlTypes as PostgresqlTypes
import Test.Hspec
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QuickCheck
import Prelude

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
  let _typeName = untag (PostgresqlTypes.typeName @a)
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
                            ("SELECT (?)")
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
                            ("SELECT (?)")
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
