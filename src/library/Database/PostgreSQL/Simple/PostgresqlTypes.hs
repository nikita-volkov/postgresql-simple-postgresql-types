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
import Database.PostgreSQL.Simple.PostgresqlTypes.ViaIsScalar (ViaIsScalar (ViaIsScalar))
import Database.PostgreSQL.Simple.ToField
import GHC.TypeLits
import PostgresqlTypes
import PostgresqlTypes.Algebra

deriving via ViaIsScalar (Bit length) instance (KnownNat length) => FromField (Bit length)

deriving via ViaIsScalar (Bit length) instance (KnownNat length) => ToField (Bit length)

deriving via ViaIsScalar (Bpchar length) instance (KnownNat length) => FromField (Bpchar length)

deriving via ViaIsScalar (Bpchar length) instance (KnownNat length) => ToField (Bpchar length)

deriving via ViaIsScalar (Numeric precision scale) instance (KnownNat precision, KnownNat scale) => FromField (Numeric precision scale)

deriving via ViaIsScalar (Numeric precision scale) instance (KnownNat precision, KnownNat scale) => ToField (Numeric precision scale)

deriving via ViaIsScalar (Varbit maxLen) instance (KnownNat maxLen) => FromField (Varbit maxLen)

deriving via ViaIsScalar (Varbit maxLen) instance (KnownNat maxLen) => ToField (Varbit maxLen)

deriving via ViaIsScalar (Varchar maxLen) instance (KnownNat maxLen) => FromField (Varchar maxLen)

deriving via ViaIsScalar (Varchar maxLen) instance (KnownNat maxLen) => ToField (Varchar maxLen)

-- | Decoder of 'Multirange' types.
--
-- Notice that \"postgresql-simple\" has an issue due to which queries producing arrays of multiranges always fail. See https://github.com/haskellari/postgresql-simple/issues/163. In other cases everything should work fine.
deriving via ViaIsScalar (Multirange a) instance (IsMultirangeElement a, Typeable a) => FromField (Multirange a)

deriving via ViaIsScalar (Multirange a) instance (IsMultirangeElement a) => ToField (Multirange a)

deriving via ViaIsScalar (Range a) instance (IsRangeElement a, Typeable a) => FromField (Range a)

deriving via ViaIsScalar (Range a) instance (IsRangeElement a) => ToField (Range a)

deriving via ViaIsScalar Bool instance FromField Bool

deriving via ViaIsScalar Bool instance ToField Bool

deriving via ViaIsScalar Box instance FromField Box

deriving via ViaIsScalar Box instance ToField Box

deriving via ViaIsScalar Bytea instance FromField Bytea

deriving via ViaIsScalar Bytea instance ToField Bytea

deriving via ViaIsScalar Char instance FromField Char

deriving via ViaIsScalar Char instance ToField Char

deriving via ViaIsScalar Cidr instance FromField Cidr

deriving via ViaIsScalar Cidr instance ToField Cidr

deriving via ViaIsScalar Circle instance FromField Circle

deriving via ViaIsScalar Circle instance ToField Circle

deriving via ViaIsScalar Date instance FromField Date

deriving via ViaIsScalar Date instance ToField Date

deriving via ViaIsScalar Float4 instance FromField Float4

deriving via ViaIsScalar Float4 instance ToField Float4

deriving via ViaIsScalar Float8 instance FromField Float8

deriving via ViaIsScalar Float8 instance ToField Float8

deriving via ViaIsScalar Hstore instance FromField Hstore

deriving via ViaIsScalar Hstore instance ToField Hstore

deriving via ViaIsScalar Inet instance FromField Inet

deriving via ViaIsScalar Inet instance ToField Inet

deriving via ViaIsScalar Int2 instance FromField Int2

deriving via ViaIsScalar Int2 instance ToField Int2

deriving via ViaIsScalar Int4 instance FromField Int4

deriving via ViaIsScalar Int4 instance ToField Int4

deriving via ViaIsScalar Int8 instance FromField Int8

deriving via ViaIsScalar Int8 instance ToField Int8

deriving via ViaIsScalar Interval instance FromField Interval

deriving via ViaIsScalar Interval instance ToField Interval

deriving via ViaIsScalar Json instance FromField Json

deriving via ViaIsScalar Json instance ToField Json

deriving via ViaIsScalar Jsonb instance FromField Jsonb

deriving via ViaIsScalar Jsonb instance ToField Jsonb

deriving via ViaIsScalar Line instance FromField Line

deriving via ViaIsScalar Line instance ToField Line

deriving via ViaIsScalar Lseg instance FromField Lseg

deriving via ViaIsScalar Lseg instance ToField Lseg

deriving via ViaIsScalar Macaddr instance FromField Macaddr

deriving via ViaIsScalar Macaddr instance ToField Macaddr

deriving via ViaIsScalar Macaddr8 instance FromField Macaddr8

deriving via ViaIsScalar Macaddr8 instance ToField Macaddr8

deriving via ViaIsScalar Money instance FromField Money

deriving via ViaIsScalar Money instance ToField Money

deriving via ViaIsScalar Oid instance FromField Oid

deriving via ViaIsScalar Oid instance ToField Oid

deriving via ViaIsScalar Path instance FromField Path

deriving via ViaIsScalar Path instance ToField Path

deriving via ViaIsScalar Point instance FromField Point

deriving via ViaIsScalar Point instance ToField Point

deriving via ViaIsScalar Polygon instance FromField Polygon

deriving via ViaIsScalar Polygon instance ToField Polygon

deriving via ViaIsScalar Text instance FromField Text

deriving via ViaIsScalar Text instance ToField Text

deriving via ViaIsScalar Time instance FromField Time

deriving via ViaIsScalar Time instance ToField Time

deriving via ViaIsScalar Timestamp instance FromField Timestamp

deriving via ViaIsScalar Timestamp instance ToField Timestamp

deriving via ViaIsScalar Timestamptz instance FromField Timestamptz

deriving via ViaIsScalar Timestamptz instance ToField Timestamptz

deriving via ViaIsScalar Timetz instance FromField Timetz

deriving via ViaIsScalar Timetz instance ToField Timetz

deriving via ViaIsScalar Uuid instance FromField Uuid

deriving via ViaIsScalar Uuid instance ToField Uuid
