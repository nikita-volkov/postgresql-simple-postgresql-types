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
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.PostgresqlTypes.ViaIsPrimitive (ViaIsPrimitive (ViaIsPrimitive))
import Database.PostgreSQL.Simple.ToField
import GHC.TypeLits
import PostgresqlTypes
import PostgresqlTypes.Algebra

deriving via ViaIsPrimitive (Bit length) instance (KnownNat length) => FromField (Bit length)

deriving via ViaIsPrimitive (Bit length) instance (KnownNat length) => ToField (Bit length)

deriving via ViaIsPrimitive (Bpchar length) instance (KnownNat length) => FromField (Bpchar length)

deriving via ViaIsPrimitive (Bpchar length) instance (KnownNat length) => ToField (Bpchar length)

deriving via ViaIsPrimitive (Numeric precision scale) instance (KnownNat precision, KnownNat scale) => FromField (Numeric precision scale)

deriving via ViaIsPrimitive (Numeric precision scale) instance (KnownNat precision, KnownNat scale) => ToField (Numeric precision scale)

deriving via ViaIsPrimitive (Varbit maxLen) instance (KnownNat maxLen) => FromField (Varbit maxLen)

deriving via ViaIsPrimitive (Varbit maxLen) instance (KnownNat maxLen) => ToField (Varbit maxLen)

deriving via ViaIsPrimitive (Varchar maxLen) instance (KnownNat maxLen) => FromField (Varchar maxLen)

deriving via ViaIsPrimitive (Varchar maxLen) instance (KnownNat maxLen) => ToField (Varchar maxLen)

-- | Decoder of 'Multirange' types.
--
-- Notice that \"postgresql-simple\" has an issue due to which queries producing arrays of multiranges always fail. See https://github.com/haskellari/postgresql-simple/issues/163. In other cases everything should work fine.
deriving via ViaIsPrimitive (Multirange a) instance (IsMultirangeElement a, Typeable a) => FromField (Multirange a)

deriving via ViaIsPrimitive (Multirange a) instance (IsMultirangeElement a) => ToField (Multirange a)

deriving via ViaIsPrimitive (Range a) instance (IsRangeElement a, Typeable a) => FromField (Range a)

deriving via ViaIsPrimitive (Range a) instance (IsRangeElement a) => ToField (Range a)

deriving via ViaIsPrimitive Bool instance FromField Bool

deriving via ViaIsPrimitive Bool instance ToField Bool

deriving via ViaIsPrimitive Box instance FromField Box

deriving via ViaIsPrimitive Box instance ToField Box

deriving via ViaIsPrimitive Bytea instance FromField Bytea

deriving via ViaIsPrimitive Bytea instance ToField Bytea

deriving via ViaIsPrimitive Char instance FromField Char

deriving via ViaIsPrimitive Char instance ToField Char

deriving via ViaIsPrimitive Cidr instance FromField Cidr

deriving via ViaIsPrimitive Cidr instance ToField Cidr

deriving via ViaIsPrimitive Circle instance FromField Circle

deriving via ViaIsPrimitive Circle instance ToField Circle

deriving via ViaIsPrimitive Citext instance FromField Citext

deriving via ViaIsPrimitive Citext instance ToField Citext

deriving via ViaIsPrimitive Date instance FromField Date

deriving via ViaIsPrimitive Date instance ToField Date

deriving via ViaIsPrimitive Float4 instance FromField Float4

deriving via ViaIsPrimitive Float4 instance ToField Float4

deriving via ViaIsPrimitive Float8 instance FromField Float8

deriving via ViaIsPrimitive Float8 instance ToField Float8

-- | Decoder of the 'Geometry' PostGIS type.
--
-- Requires the @postgis@ extension to be installed in PostgreSQL.
deriving via ViaIsPrimitive Geometry instance FromField Geometry

deriving via ViaIsPrimitive Geometry instance ToField Geometry

deriving via ViaIsPrimitive Hstore instance FromField Hstore

deriving via ViaIsPrimitive Hstore instance ToField Hstore

deriving via ViaIsPrimitive Inet instance FromField Inet

deriving via ViaIsPrimitive Inet instance ToField Inet

deriving via ViaIsPrimitive Int2 instance FromField Int2

deriving via ViaIsPrimitive Int2 instance ToField Int2

deriving via ViaIsPrimitive Int4 instance FromField Int4

deriving via ViaIsPrimitive Int4 instance ToField Int4

deriving via ViaIsPrimitive Int8 instance FromField Int8

deriving via ViaIsPrimitive Int8 instance ToField Int8

deriving via ViaIsPrimitive Interval instance FromField Interval

deriving via ViaIsPrimitive Interval instance ToField Interval

deriving via ViaIsPrimitive Json instance FromField Json

deriving via ViaIsPrimitive Json instance ToField Json

deriving via ViaIsPrimitive Jsonb instance FromField Jsonb

deriving via ViaIsPrimitive Jsonb instance ToField Jsonb

deriving via ViaIsPrimitive Line instance FromField Line

deriving via ViaIsPrimitive Line instance ToField Line

deriving via ViaIsPrimitive Lseg instance FromField Lseg

deriving via ViaIsPrimitive Lseg instance ToField Lseg

deriving via ViaIsPrimitive Macaddr instance FromField Macaddr

deriving via ViaIsPrimitive Macaddr instance ToField Macaddr

deriving via ViaIsPrimitive Macaddr8 instance FromField Macaddr8

deriving via ViaIsPrimitive Macaddr8 instance ToField Macaddr8

deriving via ViaIsPrimitive Money instance FromField Money

deriving via ViaIsPrimitive Money instance ToField Money

deriving via ViaIsPrimitive Oid instance FromField Oid

deriving via ViaIsPrimitive Oid instance ToField Oid

deriving via ViaIsPrimitive Path instance FromField Path

deriving via ViaIsPrimitive Path instance ToField Path

deriving via ViaIsPrimitive Point instance FromField Point

deriving via ViaIsPrimitive Point instance ToField Point

deriving via ViaIsPrimitive Polygon instance FromField Polygon

deriving via ViaIsPrimitive Polygon instance ToField Polygon

deriving via ViaIsPrimitive Text instance FromField Text

deriving via ViaIsPrimitive Text instance ToField Text

deriving via ViaIsPrimitive Time instance FromField Time

deriving via ViaIsPrimitive Time instance ToField Time

deriving via ViaIsPrimitive Timestamp instance FromField Timestamp

deriving via ViaIsPrimitive Timestamp instance ToField Timestamp

deriving via ViaIsPrimitive Timestamptz instance FromField Timestamptz

deriving via ViaIsPrimitive Timestamptz instance ToField Timestamptz

deriving via ViaIsPrimitive Timetz instance FromField Timetz

deriving via ViaIsPrimitive Timetz instance ToField Timetz

deriving via ViaIsPrimitive Tsvector instance FromField Tsvector

deriving via ViaIsPrimitive Tsvector instance ToField Tsvector

deriving via ViaIsPrimitive Uuid instance FromField Uuid

deriving via ViaIsPrimitive Uuid instance ToField Uuid
