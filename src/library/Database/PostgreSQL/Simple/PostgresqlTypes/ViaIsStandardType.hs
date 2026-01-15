{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module provides a bridge between PostgreSQL's standard types and the postgresql-simple library,
-- offering automatic ToField and FromField instance generation for types that implement the 'IsStandardType' constraint.
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
-- * 'toFieldVia' creates a 'ToField' compatible 'Action' using the 'textualEncoder' from 'Pt.IsStandardType'
-- * 'fromFieldVia' creates a 'FromField' compatible parser using the 'textualDecoder' from 'Pt.IsStandardType'
--
-- The module uses textual format for encoding/decoding since that's what postgresql-simple primarily uses.
module Database.PostgreSQL.Simple.PostgresqlTypes.ViaIsStandardType (ViaIsStandardType (..)) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.PostgresqlTypes.Prelude
import Database.PostgreSQL.Simple.ToField
import qualified PostgresqlTypes as Pt
import qualified TextBuilder

newtype ViaIsStandardType a = ViaIsStandardType a

instance (Pt.IsStandardType a) => ToField (ViaIsStandardType a) where
  toField (ViaIsStandardType value) = toFieldVia value

instance (Typeable a, Pt.IsStandardType a) => FromField (ViaIsStandardType a) where
  fromField field mdata = ViaIsStandardType <$> fromFieldVia field mdata

-- | Convert a postgresql-types value to a postgresql-simple 'Action'.
--
-- This function uses the textual encoder from 'IsStandardType' to produce
-- an escaped text value suitable for use in SQL queries.
--
-- > instance ToField Int4 where
-- >   toField = toFieldVia
toFieldVia :: forall a. (Pt.IsStandardType a) => a -> Action
toFieldVia value =
  Escape (TextEncoding.encodeUtf8 (TextBuilder.toText (Pt.textualEncoder value)))

-- | Parse a postgresql-types value from a postgresql-simple field.
--
-- This function uses the textual decoder from 'Pt.IsStandardType' to parse
-- values received from PostgreSQL in text format.
--
-- It validates the field's type by comparing:
-- * The field's OID against the type's expected base OID or array OID
-- * When OID is not statically known, falls back to comparing type names
-- * Automatically handles array types by checking against arrayOid
--
-- > instance FromField Int4 where
-- >   fromField = fromFieldVia
fromFieldVia :: forall a. (Typeable a, Pt.IsStandardType a) => FieldParser a
fromFieldVia field mdata = do
  -- Type validation: check OID or name
  let expectedBaseOid = untag (Pt.baseOid @a)
      expectedArrayOid = untag (Pt.arrayOid @a)
      expectedTypeName = untag (Pt.typeName @a)
      fieldOid = typeOid field

  fieldTypeName <- typename field

  -- Check if this is an array type by looking at the field's typename
  -- PostgreSQL array types start with '_'
  let isArrayField =
        let nameText = TextEncoding.decodeUtf8With TextEncoding.lenientDecode fieldTypeName
         in Text.take 1 nameText == "_"

      typeMatches = case (isArrayField, expectedArrayOid, expectedBaseOid) of
        -- For array fields, check against arrayOid
        (True, Just arrOid, _) -> fieldOid == Oid (fromIntegral arrOid)
        -- For non-array fields, check against baseOid
        (False, _, Just oid) -> fieldOid == Oid (fromIntegral oid)
        -- Fallback to typename comparison when OID not available
        _ ->
          let expectedName = TextEncoding.encodeUtf8 (if isArrayField then "_" <> expectedTypeName else expectedTypeName)
           in fieldTypeName == expectedName

  unless typeMatches do
    returnError Incompatible field
      $ mconcat
        [ "Type mismatch: expected ",
          Text.unpack expectedTypeName,
          " (OID ",
          maybe "unknown" show expectedBaseOid,
          case expectedArrayOid of
            Just arrOid -> ", array OID " <> show arrOid
            Nothing -> "",
          ") but got field with OID ",
          show fieldOid
        ]

  -- Data validation and parsing
  case mdata of
    Nothing -> returnError UnexpectedNull field ""
    Just bytes -> case TextEncoding.decodeUtf8' bytes of
      Left err ->
        returnError ConversionFailed field
          $ "UTF-8 decoding failed: "
          <> show err
      Right text ->
        case Attoparsec.parseOnly (Pt.textualDecoder @a <* Attoparsec.endOfInput) text of
          Left err ->
            returnError ConversionFailed field
              $ "Parsing failed: "
              <> err
          Right value -> pure value
