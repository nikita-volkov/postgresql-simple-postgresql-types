{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module provides a bridge between PostgreSQL's standard types and the "postgresql-simple" library,
-- offering 'ToField' and 'FromField' instances for types defined in the "postgresql-types" library.
--
-- = Usage
--
-- Import this module in addition to @Database.PostgreSQL.Simple@ to get encoding/decoding support
-- for postgresql-types in postgresql-simple queries:
--
-- > import Database.PostgreSQL.Simple
-- > import Database.PostgreSQL.Simple.PostgresqlTypes
-- > import PostgresqlTypes.Types qualified as Pt
-- >
-- > -- Now you can use postgresql-types directly in queries
-- > example :: Connection -> Pt.Int4 -> IO [Only Pt.Text]
-- > example conn myInt = query conn "SELECT name FROM users WHERE id = ?" (Only myInt)
module Database.PostgreSQL.Simple.PostgresqlTypes
  (
  )
where

import Data.Data (Typeable)
import Data.Monoid
import qualified Data.Text.Encoding as TextEncoding
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.PostgresqlTypes.ViaIsStandardType (ViaIsStandardType (ViaIsStandardType))
import Database.PostgreSQL.Simple.ToField
import PostgresqlTypes
import qualified TextBuilder

instance ToField Bit where
  toField value =
    Plain
      ( TextEncoding.encodeUtf8Builder
          ( TextBuilder.toText
              ( mconcat
                  [ "B'",
                    textualEncoder value,
                    "'"
                  ]
              )
          )
      )

deriving via ViaIsStandardType Bit instance FromField Bit

deriving via ViaIsStandardType Varbit instance ToField Varbit

deriving via ViaIsStandardType Varbit instance FromField Varbit

deriving via ViaIsStandardType Bool instance ToField Bool

deriving via ViaIsStandardType Bool instance FromField Bool

deriving via ViaIsStandardType Box instance ToField Box

deriving via ViaIsStandardType Box instance FromField Box

deriving via ViaIsStandardType Circle instance ToField Circle

deriving via ViaIsStandardType Circle instance FromField Circle

deriving via ViaIsStandardType Line instance ToField Line

deriving via ViaIsStandardType Line instance FromField Line

deriving via ViaIsStandardType Lseg instance ToField Lseg

deriving via ViaIsStandardType Lseg instance FromField Lseg

deriving via ViaIsStandardType Path instance ToField Path

deriving via ViaIsStandardType Path instance FromField Path

deriving via ViaIsStandardType Point instance ToField Point

deriving via ViaIsStandardType Point instance FromField Point

deriving via ViaIsStandardType Polygon instance ToField Polygon

deriving via ViaIsStandardType Polygon instance FromField Polygon

deriving via ViaIsStandardType Bytea instance ToField Bytea

deriving via ViaIsStandardType Bytea instance FromField Bytea

deriving via ViaIsStandardType Char instance ToField Char

deriving via ViaIsStandardType Char instance FromField Char

deriving via ViaIsStandardType Text instance ToField Text

deriving via ViaIsStandardType Text instance FromField Text

deriving via ViaIsStandardType Varchar instance ToField Varchar

deriving via ViaIsStandardType Varchar instance FromField Varchar

deriving via ViaIsStandardType Cidr instance ToField Cidr

deriving via ViaIsStandardType Cidr instance FromField Cidr

deriving via ViaIsStandardType Inet instance ToField Inet

deriving via ViaIsStandardType Inet instance FromField Inet

deriving via ViaIsStandardType Macaddr instance ToField Macaddr

deriving via ViaIsStandardType Macaddr instance FromField Macaddr

deriving via ViaIsStandardType Macaddr8 instance ToField Macaddr8

deriving via ViaIsStandardType Macaddr8 instance FromField Macaddr8

deriving via ViaIsStandardType Date instance ToField Date

deriving via ViaIsStandardType Date instance FromField Date

deriving via ViaIsStandardType Time instance ToField Time

deriving via ViaIsStandardType Time instance FromField Time

deriving via ViaIsStandardType Timestamp instance ToField Timestamp

deriving via ViaIsStandardType Timestamp instance FromField Timestamp

deriving via ViaIsStandardType Timestamptz instance ToField Timestamptz

deriving via ViaIsStandardType Timestamptz instance FromField Timestamptz

deriving via ViaIsStandardType Timetz instance ToField Timetz

deriving via ViaIsStandardType Timetz instance FromField Timetz

deriving via ViaIsStandardType TimetzAsTimeOfDayAndTimeZone instance ToField TimetzAsTimeOfDayAndTimeZone

deriving via ViaIsStandardType TimetzAsTimeOfDayAndTimeZone instance FromField TimetzAsTimeOfDayAndTimeZone

deriving via ViaIsStandardType Interval instance ToField Interval

deriving via ViaIsStandardType Interval instance FromField Interval

deriving via ViaIsStandardType IntervalAsMicroseconds instance ToField IntervalAsMicroseconds

deriving via ViaIsStandardType IntervalAsMicroseconds instance FromField IntervalAsMicroseconds

deriving via ViaIsStandardType Float4 instance ToField Float4

deriving via ViaIsStandardType Float4 instance FromField Float4

deriving via ViaIsStandardType Float8 instance ToField Float8

deriving via ViaIsStandardType Float8 instance FromField Float8

deriving via ViaIsStandardType Int2 instance ToField Int2

deriving via ViaIsStandardType Int2 instance FromField Int2

deriving via ViaIsStandardType Int4 instance ToField Int4

deriving via ViaIsStandardType Int4 instance FromField Int4

deriving via ViaIsStandardType Int8 instance ToField Int8

deriving via ViaIsStandardType Int8 instance FromField Int8

deriving via ViaIsStandardType Money instance ToField Money

deriving via ViaIsStandardType Money instance FromField Money

deriving via ViaIsStandardType Numeric instance ToField Numeric

deriving via ViaIsStandardType Numeric instance FromField Numeric

deriving via ViaIsStandardType Oid instance ToField Oid

deriving via ViaIsStandardType Oid instance FromField Oid

deriving via ViaIsStandardType Hstore instance ToField Hstore

deriving via ViaIsStandardType Hstore instance FromField Hstore

deriving via ViaIsStandardType Json instance ToField Json

deriving via ViaIsStandardType Json instance FromField Json

deriving via ViaIsStandardType Jsonb instance ToField Jsonb

deriving via ViaIsStandardType Jsonb instance FromField Jsonb

deriving via ViaIsStandardType (Range a) instance (IsRangeElement a) => ToField (Range a)

deriving via ViaIsStandardType (Range a) instance (IsRangeElement a, Typeable a) => FromField (Range a)

deriving via ViaIsStandardType (Multirange a) instance (IsMultirangeElement a) => ToField (Multirange a)

deriving via ViaIsStandardType (Multirange a) instance (IsMultirangeElement a, Typeable a) => FromField (Multirange a)

deriving via ViaIsStandardType Uuid instance ToField Uuid

deriving via ViaIsStandardType Uuid instance FromField Uuid
