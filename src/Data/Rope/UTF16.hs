module Data.Rope.UTF16
  ( Rope.Rope

  -- * Queries
  , Rope.null
  , Rope.length
  , Rope.rows
  , Rope.columns

  -- * Conversions to and from 'Text' and 'String'
  , Rope.toText
  , Rope.toLazyText
  , Rope.fromText
  , Rope.toString

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

  -- * Breaking by predicate
  , Rope.span
  , Rope.break
  , Rope.takeWhile
  , Rope.dropWhile
  ) where

import Data.Rope.UTF16.Internal as Rope
import Data.Rope.UTF16.Internal.Position as Position
