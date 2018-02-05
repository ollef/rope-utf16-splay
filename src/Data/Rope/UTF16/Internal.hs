{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
module Data.Rope.UTF16.Internal where

import Data.FingerTree(FingerTree, ViewL(EmptyL, (:<)), ViewR(EmptyR, (:>)))
import qualified Data.FingerTree as FingerTree
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

instance FingerTree.Measured Position Chunk where
  measure (Chunk _ m) = m

-- | A @FingerTree@ of @Text@ values optimised for being indexed by and
-- modified at UTF-16 code points and row/column (@RowColumn@) positions.
-- Internal invariant: No empty @Chunk@s in the @FingerTree@
newtype Rope = Rope { unrope :: FingerTree Position Chunk }
  deriving (FingerTree.Measured Position, Show)

-- | The maximum length, in code points, of a chunk
chunkLength :: Int
chunkLength = 1000

-- | Append joins adjacent chunks if that can be done while staying below
-- @chunkLength@.
instance Semigroup Rope where
  Rope r1 <> Rope r2 = case (FingerTree.viewr r1, FingerTree.viewl r2) of
    (EmptyR, _) -> Rope r2
    (_, EmptyL) -> Rope r1
    (r1' :> a, b :< r2')
      | codePoints (chunkMeasure a <> chunkMeasure b) <= chunkLength
        -> Rope $ r1' <> FingerTree.singleton (a <> b) <> r2'
      | otherwise
        -> Rope $ r1 <> r2

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
  | otherwise
    = Rope
    $ mconcat
    $ FingerTree.singleton . chunk <$> chunks16Of chunkLength t

fromShortText :: Text -> Rope
fromShortText t
  | Text.null t = mempty
  | otherwise = Rope $ FingerTree.singleton $ chunk t

-------------------------------------------------------------------------------
-- * Chunking

-- | The raw @Text@ data that the @Rope@ is built from
toChunks :: Rope -> [Text]
toChunks = fmap chunkText . toList . unrope

-- | Get the first chunk and the rest of the @Rope@ if non-empty
unconsChunk :: Rope -> Maybe (Text, Rope)
unconsChunk (Rope r) = case FingerTree.viewl r of
  EmptyL -> Nothing
  Chunk t _ :< r' -> Just (t, Rope r')

-- | Get the last chunk and the rest of the @Rope@ if non-empty
unsnocChunk :: Rope -> Maybe (Rope, Text)
unsnocChunk (Rope r) = case FingerTree.viewr r of
  EmptyR -> Nothing
  r' :> Chunk t _ -> Just (Rope r', t)

-------------------------------------------------------------------------------
-- * UTF-16 code point indexing

-- | Length in code points (not characters)
length :: Rope -> Int
length = codePoints . FingerTree.measure

-- | Split the rope at the nth code point (not character)
splitAt :: Int -> Rope -> (Rope, Rope)
splitAt n (Rope r)
  | n <= 0 = (mempty, Rope r)
  | otherwise = case FingerTree.viewl post of
    EmptyL -> (Rope pre, Rope post)
    Chunk t _ :< post' ->
      (Rope pre <> fromShortText pret, fromShortText postt <> Rope post')
      where
        n' = n - codePoints (FingerTree.measure pre)
        (pret, postt) = split16At n' t
    where
      (pre, post) = FingerTree.split ((> n) . codePoints) r

-- | Take the first n code points (not characters)
take :: Int -> Rope -> Rope
take n = fst . Data.Rope.UTF16.Internal.splitAt n

-- | Drop the first n code points (not characters)
drop :: Int -> Rope -> Rope
drop n = snd . Data.Rope.UTF16.Internal.splitAt n

-- | Get the code point index in the rope that corresponds to a @RowColumn@ position
rowColumnCodePoints :: RowColumn -> Rope -> Int
rowColumnCodePoints v (Rope r) = case FingerTree.viewl post of
  EmptyL -> codePoints prePos
  Chunk t _ :< _ -> go 0 $ rowColumn prePos
    where
      len = Unsafe.lengthWord16 t
      go i !v'
        | v <= v' || i >= len = codePoints prePos + i
        | otherwise = case Unsafe.iter t i of
          Unsafe.Iter '\n' delta -> go (i + delta) (v' <> RowColumn 1 0)
          Unsafe.Iter _ 2 | v == v' <> RowColumn 0 1 -> codePoints prePos + i
          Unsafe.Iter _ delta -> go (i + delta) (v' <> RowColumn 0 delta)
  where
    (pre, post) = FingerTree.split ((> v) . rowColumn) r
    prePos = FingerTree.measure pre

-------------------------------------------------------------------------------
-- * Breaking by predicate

span :: (Char -> Bool) -> Rope -> (Rope, Rope)
span f (Rope r) = case FingerTree.viewl r of
  EmptyL -> (mempty, mempty)
  t :< r'
    | Text.null postt -> (Rope (FingerTree.singleton t) <> pre', post')
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
