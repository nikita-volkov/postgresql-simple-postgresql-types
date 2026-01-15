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

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.PostgresqlTypes.ViaIsStandardType (ViaIsStandardType (ViaIsStandardType))
import Database.PostgreSQL.Simple.ToField (ToField)
import PostgresqlTypes.Types

deriving via ViaIsStandardType Bit instance ToField Bit

deriving via ViaIsStandardType Bit instance FromField Bit
