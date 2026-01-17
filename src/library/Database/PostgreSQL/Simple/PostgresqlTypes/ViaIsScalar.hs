{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module provides a bridge between PostgreSQL's standard types and the postgresql-simple library,
-- offering automatic ToField and FromField instance generation for types that implement the 'IsScalar' constraint.
--
-- = Usage
--
-- Import this module in addition to @Database.PostgreSQL.Simple@ to get encoding/decoding support
-- for postgresql-types in postgresql-simple queries:
--
-- > import Database.PostgreSQL.Simple
-- > import Database.PostgreSQL.Simple.PostgresqlTypes
-- > import PostgresqlTypes.Types (Int4, Text)
-- >
-- > -- Now you can use postgresql-types directly in queries
-- > example :: Connection -> Int4 -> IO [Only Text]
-- > example conn myInt = query conn "SELECT name FROM users WHERE id = ?" (Only myInt)
--
-- = How it works
--
-- * 'toFieldVia' creates a 'ToField' compatible 'Action' using the 'textualEncoder' from 'Pta.IsScalar'
-- * 'fromFieldVia' creates a 'FromField' compatible parser using the 'textualDecoder' from 'Pta.IsScalar'
--
-- The module uses textual format for encoding/decoding since that's what postgresql-simple primarily uses.
module Database.PostgreSQL.Simple.PostgresqlTypes.ViaIsScalar (ViaIsScalar (..)) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.PostgresqlTypes.Prelude
import Database.PostgreSQL.Simple.ToField
import qualified PostgresqlTypes.Algebra as Pta
import qualified TextBuilder

newtype ViaIsScalar a = ViaIsScalar a

instance (Pta.IsScalar a) => ToField (ViaIsScalar a) where
  toField (ViaIsScalar value) = toFieldVia value

instance (Typeable a, Pta.IsScalar a) => FromField (ViaIsScalar a) where
  fromField field mdata = ViaIsScalar <$> fromFieldVia field mdata

-- | Convert a postgresql-types value to a postgresql-simple 'Action'.
--
-- This function uses the textual encoder from 'IsScalar' to produce
-- an escaped text value suitable for use in SQL queries.
--
-- > instance ToField Int4 where
-- >   toField = toFieldVia
toFieldVia :: forall a. (Pta.IsScalar a) => a -> Action
toFieldVia value =
  Many
    [ Escape (TextEncoding.encodeUtf8 (TextBuilder.toText (Pta.textualEncoder value))),
      Plain ("::" <> TextEncoding.encodeUtf8Builder (untag (Pta.typeSignature @a)))
    ]

-- | Parse a postgresql-types value from a postgresql-simple field.
--
-- This function uses the textual decoder from 'Pta.IsScalar' to parse
-- values received from PostgreSQL in text format.
--
-- It validates the field's type by comparing:
-- * The field's OID against the type's expected base OID or array OID
-- * When OID is not statically known, falls back to comparing type names
-- * Automatically handles array types by checking against arrayOid
--
-- > instance FromField Int4 where
-- >   fromField = fromFieldVia
fromFieldVia :: forall a. (Typeable a, Pta.IsScalar a) => FieldParser a
fromFieldVia field mdata = do
  -- Type validation: check OID or name
  let expectedBaseOid = untag (Pta.baseOid @a)
      expectedArrayOid = untag (Pta.arrayOid @a)
      expectedTypeName = untag (Pta.typeName @a)
      fieldOid = typeOid field

  case (expectedBaseOid, expectedArrayOid) of
    -- For types with known OIDs, validate against OID without calling typename
    (Just expectedBaseOid, Just expectedArrayOid) -> do
      let typeMatches =
            fieldOid == Oid (fromIntegral expectedBaseOid)
              || fieldOid == Oid (fromIntegral expectedArrayOid)

      unless typeMatches do
        returnError Incompatible field $
          mconcat
            [ "Type mismatch: expected ",
              Text.unpack expectedTypeName,
              " (OID ",
              show expectedBaseOid,
              ", array OID " <> show expectedArrayOid,
              ") but got field with OID ",
              show fieldOid
            ]

    -- Only call typename if we need it for validation (when OID is not available)
    _ -> do
      fieldTypeName <- typename field
      let expectedName = TextEncoding.encodeUtf8 expectedTypeName
      unless (fieldTypeName == expectedName) do
        returnError Incompatible field $
          mconcat
            [ "Type mismatch: expected ",
              Text.unpack expectedTypeName,
              " (OID unknown) but got field with type name ",
              show (TextEncoding.decodeUtf8With TextEncoding.lenientDecode fieldTypeName)
            ]

  -- Data validation and parsing
  case mdata of
    Nothing -> returnError UnexpectedNull field ""
    Just bytes -> case TextEncoding.decodeUtf8' bytes of
      Left err ->
        returnError ConversionFailed field $
          "UTF-8 decoding failed: " <> show err
      Right text ->
        case Attoparsec.parseOnly (Pta.textualDecoder @a <* Attoparsec.endOfInput) text of
          Left err ->
            returnError ConversionFailed field $
              "Parsing failed: " <> err
          Right value -> pure value
