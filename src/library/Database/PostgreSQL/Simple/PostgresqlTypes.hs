{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

-- |
-- This module provides a bridge between PostgreSQL's standard types and the "postgresql-simple" library,
-- offering automatic codec generation for types that implement the 'PostgresqlTypes.IsStandardType' constraint.
module Database.PostgreSQL.Simple.PostgresqlTypes
  ( IsStandardType,
  )
where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.PostgresqlTypes.Prelude
import PostgresqlTypes
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
