module Data.Rope.UTF16
  ( Rope.Rope

  -- * Queries
  , Rope.null
  , Rope.length
  , Rope.rows
  , Rope.columns

  -- * Conversions
  , Rope.toText
  , Rope.toLazyText
  , Rope.fromText
  , Rope.toString

  -- * Transformations
  , Rope.map
  , Rope.intercalate

  -- * Chunking
  , Rope.toChunks
  , Rope.unconsChunk
  , Rope.unsnocChunk

  -- * UTF-16 code unit indexing
  , Rope.splitAt
  , Rope.take
  , Rope.drop
  , Position.RowColumn(..)
  , Rope.rowColumnCodeUnits
  , Rope.splitAtLine

  -- * Breaking by predicate
  , Rope.span
  , Rope.break
  , Rope.takeWhile
  , Rope.dropWhile

  -- * Folds
  , Rope.foldl
  , Rope.foldl'
  , Rope.foldr

  -- * Special folds
  , Rope.any
  , Rope.all
  ) where

import Data.Rope.UTF16.Internal as Rope
import Data.Rope.UTF16.Internal.Position as Position
