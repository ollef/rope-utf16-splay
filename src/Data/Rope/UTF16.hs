module Data.Rope.UTF16
  ( module Position
  , Rope.Rope

  -- * Conversions to and from @Text@
  , Rope.toText
  , Rope.fromText

  -- * Chunking
  , Rope.toChunks
  , Rope.unconsChunk
  , Rope.unsnocChunk

  -- * UTF-16 code point indexing
  , Rope.length
  , Rope.splitAt
  , Rope.take
  , Rope.drop
  , Rope.rowColumnCodePoints

  -- * Breaking by predicate
  , Rope.span
  , Rope.break
  , Rope.takeWhile
  , Rope.dropWhile
  ) where

import Data.Rope.UTF16.Position as Position
import Data.Rope.UTF16.Internal as Rope
