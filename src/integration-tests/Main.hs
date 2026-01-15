module Main (main) where

import Database.PostgreSQL.Simple.PostgresqlTypes ()
import Main.Helpers
import qualified PostgresqlTypes.Types as Pt
import Test.Hspec
import Test.QuickCheck.Instances ()
import Prelude

main :: IO ()
main =
  hspec do
    parallel do
      withContainer "postgres:17" do
        withConnection do
          withType @Pt.Bit [mappingSpec]
          withType @Pt.Bool [mappingSpec]
          withType @Pt.Box [mappingSpec]
          withType @Pt.Bytea [mappingSpec]
          withType @Pt.Char [mappingSpec]
          withType @Pt.Cidr [mappingSpec]
          withType @Pt.Circle [mappingSpec]
          withType @Pt.Date [mappingSpec]
          withType @Pt.Float4 [mappingSpec]
          withType @Pt.Float8 [mappingSpec]
          withType @Pt.Hstore [mappingSpec]
          withType @Pt.Inet [mappingSpec]
          withType @Pt.Int2 [mappingSpec]
          withType @Pt.Int4 [mappingSpec]
          withType @Pt.Int8 [mappingSpec]
          withType @Pt.Interval [mappingSpec]
          withType @Pt.IntervalAsMicroseconds [mappingSpec]
          withType @Pt.Json [mappingSpec]
          withType @Pt.Jsonb [mappingSpec]
          withType @Pt.Line [mappingSpec]
          withType @Pt.Lseg [mappingSpec]
          withType @Pt.Macaddr [mappingSpec]
          withType @Pt.Macaddr8 [mappingSpec]
          withType @Pt.Money [mappingSpec]
          withType @Pt.Numeric [mappingSpec]
          withType @Pt.Oid [mappingSpec]
          withType @Pt.Path [mappingSpec]
          withType @Pt.Point [mappingSpec]
          withType @Pt.Polygon [mappingSpec]
          withType @(Pt.Range Pt.Int4) [mappingSpec]
          withType @(Pt.Range Pt.Int8) [mappingSpec]
          withType @(Pt.Range Pt.Numeric) [mappingSpec]
          withType @(Pt.Range Pt.Timestamp) [mappingSpec]
          withType @(Pt.Range Pt.Timestamptz) [mappingSpec]
          withType @(Pt.Range Pt.Date) [mappingSpec]
          withType @(Pt.Multirange Pt.Int4) [mappingSpec]
          withType @(Pt.Multirange Pt.Int8) [mappingSpec]
          withType @(Pt.Multirange Pt.Numeric) [mappingSpec]
          withType @(Pt.Multirange Pt.Timestamp) [mappingSpec]
          withType @(Pt.Multirange Pt.Timestamptz) [mappingSpec]
          withType @(Pt.Multirange Pt.Date) [mappingSpec]
          withType @Pt.Text [mappingSpec]
          withType @Pt.Time [mappingSpec]
          withType @Pt.Timestamp [mappingSpec]
          withType @Pt.Timestamptz [mappingSpec]
          withType @Pt.Timetz [mappingSpec]
          withType @Pt.Uuid [mappingSpec]
          withType @Pt.Varbit [mappingSpec]
          withType @Pt.Varchar [mappingSpec]
