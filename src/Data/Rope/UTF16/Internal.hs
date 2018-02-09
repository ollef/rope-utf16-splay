{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
module Data.Rope.UTF16.Internal where

import Data.Foldable
import Data.Function
import Data.Semigroup
import Data.String
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Unsafe as Unsafe

import Data.Rope.UTF16.Internal.Text
import Data.Rope.UTF16.Position
import Data.SplayTree(SplayTree)
import qualified Data.SplayTree as SplayTree

data Chunk = Chunk { chunkText :: !Text, chunkMeasure :: !Position }

instance Show Chunk where
  show (Chunk t _) = show t

instance Semigroup Chunk where
  Chunk t1 m1 <> Chunk t2 m2 = Chunk (t1 <> t2) (m1 <> m2)

chunk :: Text -> Chunk
chunk t = Chunk t $ Position len $ go 0 $ RowColumn 0 0
  where
    len = Unsafe.lengthWord16 t
    go i !v
      | i >= len = v
      | otherwise = case Unsafe.iter t i of
        Unsafe.Iter '\n' delta -> go (i + delta) (v <> RowColumn 1 0)
        Unsafe.Iter _ delta -> go (i + delta) (v <> RowColumn 0 delta)

instance SplayTree.Measured Position Chunk where
  measure (Chunk _ m) = m

-- | A @SplayTree@ of @Text@ values optimised for being indexed by and
-- modified at UTF-16 code points and row/column (@RowColumn@) positions.
-- Internal invariant: No empty @Chunk@s in the @SplayTree@
newtype Rope = Rope { unrope :: SplayTree Position Chunk }
  deriving (SplayTree.Measured Position, Show)

-- | The maximum length, in code points, of a chunk
chunkLength :: Int
chunkLength = 1000

-- | Append joins adjacent chunks if that can be done while staying below
-- @chunkLength@.
instance Semigroup Rope where
  Rope r1 <> Rope r2 = case (SplayTree.unsnoc r1, SplayTree.uncons r2) of
    (Nothing, _) -> Rope r2
    (_, Nothing) -> Rope r1
    (Just (r1', a), Just (b, r2'))
      | codePoints (chunkMeasure a <> chunkMeasure b) <= chunkLength
        -> Rope $ r1' <> ((a <> b) SplayTree.<| r2')
      | otherwise
        -> Rope $ r1' <> (a SplayTree.<| b SplayTree.<| r2')

instance Monoid Rope where
  mempty = Rope mempty
  mappend = (<>)

instance Eq Rope where
  (==) = (==) `on` toText

instance Ord Rope where
  compare = compare `on` toText

instance IsString Rope where
  fromString = fromText . Text.pack

-------------------------------------------------------------------------------
-- * Conversions to and from @Text@

toText :: Rope -> Text
toText = Text.concat . toChunks

toLazyText :: Rope -> Lazy.Text
toLazyText = Lazy.fromChunks . toChunks

fromText :: Text -> Rope
fromText t
  | Text.null t = mempty
  | otherwise = Rope $ go numChunks chunks
  where
    chunks = chunks16Of chunkLength t
    numChunks = Prelude.length chunks
    go !_ [] = mempty
    go len cs = SplayTree.fork (go mid pre) (chunk c) (go (len - mid - 1) post)
      where
        (pre, c:post) = Prelude.splitAt mid cs
        mid = len `div` 2

fromShortText :: Text -> Rope
fromShortText t
  | Text.null t = mempty
  | otherwise = Rope $ SplayTree.singleton $ chunk t

-------------------------------------------------------------------------------
-- * Chunking

-- | The raw @Text@ data that the @Rope@ is built from
toChunks :: Rope -> [Text]
toChunks = fmap chunkText . toList . unrope

-- | Get the first chunk and the rest of the @Rope@ if non-empty
unconsChunk :: Rope -> Maybe (Text, Rope)
unconsChunk (Rope r) = case SplayTree.uncons r of
  Nothing -> Nothing
  Just (Chunk t _, r') -> Just (t, Rope r')

-- | Get the last chunk and the rest of the @Rope@ if non-empty
unsnocChunk :: Rope -> Maybe (Rope, Text)
unsnocChunk (Rope r) = case SplayTree.unsnoc r of
  Nothing -> Nothing
  Just (r', Chunk t _) -> Just (Rope r', t)

-------------------------------------------------------------------------------
-- * UTF-16 code point indexing

-- | Length in code points (not characters)
length :: Rope -> Int
length = codePoints . SplayTree.measure

-- | Split the rope at the nth code point (not character)
splitAt :: Int -> Rope -> (Rope, Rope)
splitAt n (Rope r) = case SplayTree.split ((> n) . codePoints) r of
  SplayTree.Outside
    | n < 0 -> (mempty, Rope r)
    | otherwise -> (Rope r, mempty)
  SplayTree.Inside pre (Chunk t _) post -> (Rope pre <> fromShortText pret, fromShortText postt <> Rope post)
    where
      n' = n - codePoints (SplayTree.measure pre)
      (pret, postt) = split16At n' t

-- | Take the first n code points (not characters)
take :: Int -> Rope -> Rope
take n = fst . Data.Rope.UTF16.Internal.splitAt n

-- | Drop the first n code points (not characters)
drop :: Int -> Rope -> Rope
drop n = snd . Data.Rope.UTF16.Internal.splitAt n

-- | Get the code point index in the rope that corresponds to a @RowColumn@ position
rowColumnCodePoints :: RowColumn -> Rope -> Int
rowColumnCodePoints v (Rope r) = case SplayTree.split ((> v) . rowColumn) r of
  SplayTree.Outside
    | v <= RowColumn 0 0 -> 0
    | otherwise -> codePoints $ SplayTree.measure r
  SplayTree.Inside pre (Chunk t _) _ -> go 0 $ rowColumn prePos
    where
      prePos = SplayTree.measure pre
      len = Unsafe.lengthWord16 t
      go i !v'
        | v <= v' || i >= len = codePoints prePos + i
        | otherwise = case Unsafe.iter t i of
          Unsafe.Iter '\n' delta -> go (i + delta) (v' <> RowColumn 1 0)
          Unsafe.Iter _ 2 | v == v' <> RowColumn 0 1 -> codePoints prePos + i
          Unsafe.Iter _ delta -> go (i + delta) (v' <> RowColumn 0 delta)

-------------------------------------------------------------------------------
-- * Breaking by predicate

span :: (Char -> Bool) -> Rope -> (Rope, Rope)
span f (Rope r) = case SplayTree.uncons r of
  Nothing -> (mempty, mempty)
  Just (t, r')
    | Text.null postt -> (Rope (SplayTree.singleton t) <> pre', post')
    | otherwise -> (fromShortText pret, fromShortText postt <> Rope r')
    where
      (pret, postt) = Text.span f $ chunkText t
      (pre', post') = Data.Rope.UTF16.Internal.span f $ Rope r'

break :: (Char -> Bool) -> Rope -> (Rope, Rope)
break f = Data.Rope.UTF16.Internal.span (not . f)

takeWhile :: (Char -> Bool) -> Rope -> Rope
takeWhile f = fst . Data.Rope.UTF16.Internal.span f

dropWhile :: (Char -> Bool) -> Rope -> Rope
dropWhile f = snd . Data.Rope.UTF16.Internal.span f
