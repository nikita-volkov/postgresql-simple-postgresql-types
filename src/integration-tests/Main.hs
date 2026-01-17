module Main (main) where

import Database.PostgreSQL.Simple.PostgresqlTypes ()
import IntegrationTests.Scopes
import IntegrationTests.Scripts
import qualified PostgresqlTypes as Pt
import Test.Hspec
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main =
  hspec do
    parallel do
      withContainer "postgres:17" do
        withConnection do
          withType @(Pt.Bit 42) [mappingSpec True]
          withType @(Pt.Bpchar 42) [mappingSpec True]
          withType @(Pt.Multirange Pt.Date) [mappingSpec False]
          withType @(Pt.Multirange Pt.Int4) [mappingSpec False]
          withType @(Pt.Multirange Pt.Int8) [mappingSpec False]
          withType @(Pt.Multirange Pt.Numeric) [mappingSpec False]
          withType @(Pt.Multirange Pt.Timestamp) [mappingSpec False]
          withType @(Pt.Multirange Pt.Timestamptz) [mappingSpec False]
          withType @(Pt.Range Pt.Date) [mappingSpec True]
          withType @(Pt.Range Pt.Int4) [mappingSpec True]
          withType @(Pt.Range Pt.Int8) [mappingSpec True]
          withType @(Pt.Range Pt.Numeric) [mappingSpec True]
          withType @(Pt.Range Pt.Timestamp) [mappingSpec True]
          withType @(Pt.Range Pt.Timestamptz) [mappingSpec True]
          withType @Pt.Bool [mappingSpec True]
          withType @Pt.Box [mappingSpec True]
          withType @Pt.Bytea [mappingSpec True]
          withType @Pt.Char [mappingSpec True]
          withType @Pt.Cidr [mappingSpec True]
          withType @Pt.Circle [mappingSpec True]
          withType @Pt.Date [mappingSpec True]
          withType @Pt.Float4 [mappingSpec True]
          withType @Pt.Float8 [mappingSpec True]
          withType @Pt.Hstore [mappingSpec True]
          withType @Pt.Inet [mappingSpec True]
          withType @Pt.Int2 [mappingSpec True]
          withType @Pt.Int4 [mappingSpec True]
          withType @Pt.Int8 [mappingSpec True]
          withType @Pt.Interval [mappingSpec True]
          withType @Pt.IntervalAsMicroseconds [mappingSpec True]
          withType @Pt.Json [mappingSpec True]
          withType @Pt.Jsonb [mappingSpec True]
          withType @Pt.Line [mappingSpec True]
          withType @Pt.Lseg [mappingSpec True]
          withType @Pt.Macaddr [mappingSpec True]
          withType @Pt.Macaddr8 [mappingSpec True]
          withType @Pt.Money [mappingSpec True]
          withType @Pt.Numeric [mappingSpec True]
          withType @Pt.Oid [mappingSpec True]
          withType @Pt.Path [mappingSpec True]
          withType @Pt.Point [mappingSpec True]
          withType @Pt.Polygon [mappingSpec True]
          withType @Pt.Text [mappingSpec True]
          withType @Pt.Time [mappingSpec True]
          withType @Pt.Timestamp [mappingSpec True]
          withType @Pt.Timestamptz [mappingSpec True]
          withType @Pt.Timetz [mappingSpec True]
          withType @Pt.Uuid [mappingSpec True]
          withType @Pt.Varbit [mappingSpec True]
          withType @Pt.Varchar [mappingSpec True]
