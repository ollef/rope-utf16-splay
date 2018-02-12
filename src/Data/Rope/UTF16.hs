module Data.Rope.UTF16
  ( module Position
  , Rope.Rope

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
  , Rope.rowColumnCodeUnits

  -- * Breaking by predicate
  , Rope.span
  , Rope.break
  , Rope.takeWhile
  , Rope.dropWhile
  ) where

import Data.Rope.UTF16.Position as Position
import Data.Rope.UTF16.Internal as Rope
