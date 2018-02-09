{-# language DeriveFoldable #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
module Data.SplayTree where

import Data.Monoid
import qualified Data.Semigroup as Semigroup

infixr 5 <|
infixl 5 |>

class Monoid v => Measured v a | a -> v where
  measure :: a -> v

data SplayTree v a
  = Leaf
  | Fork (SplayTree v a) a (SplayTree v a) !v -- ^ Cached measure of the whole node
  deriving (Eq, Ord, Show, Foldable)

instance Measured v a => Measured v (SplayTree v a) where
  {-# INLINE measure #-}
  measure Leaf = mempty
  measure (Fork _ _ _ v) = v

instance Measured v a => Semigroup.Semigroup (SplayTree v a) where
  {-# INLINE (<>) #-}
  Leaf <> t = t
  t <> Leaf = t
  Fork l1 a1 r1 lar1 <> Fork l2 a2 r2 lar2
    = Fork l1 a1 (Fork (r1 <> l2) a2 r2 (measure r1 <> lar2)) (lar1 <> lar2)

instance Measured v a => Monoid (SplayTree v a) where
  {-# INLINE mempty #-}
  mempty = Leaf
  {-# INLINE mappend #-}
  mappend = (Semigroup.<>)

-------------------------------------------------------------------------------
-- * Construction

{-# INLINE singleton #-}
singleton :: Measured v a => a -> SplayTree v a
singleton a = Fork Leaf a Leaf $ measure a

{-# INLINE (<|) #-}
(<|) :: Measured v a => a -> SplayTree v a -> SplayTree v a
(<|) = fork Leaf

{-# INLINE (|>) #-}
(|>) :: Measured v a => SplayTree v a -> a -> SplayTree v a
(|>) t a = fork t a Leaf

{-# INLINE fork #-}
fork :: Measured v a => SplayTree v a -> a -> SplayTree v a -> SplayTree v a
fork l a r = Fork l a r $ measure l <> measure a <> measure r

-------------------------------------------------------------------------------
-- * Deconstruction

{-# INLINE uncons #-}
uncons :: Measured v a => SplayTree v a -> Maybe (a, SplayTree v a)
uncons Leaf = Nothing
uncons (Fork left el right _) = Just $ go left el right
  where
    go Leaf a r = (a, r)
    go (Fork l a m _) b r = go l a (fork m b r)

{-# INLINE unsnoc #-}
unsnoc :: Measured v a => SplayTree v a -> Maybe (SplayTree v a, a)
unsnoc Leaf = Nothing
unsnoc (Fork left el right _) = Just $ go left el right
  where
    go l a Leaf = (l, a)
    go l a (Fork m b r _) = go (fork l a m) b r

data SplitResult v a
  = Outside
  | Inside (SplayTree v a) a (SplayTree v a)
  deriving (Eq, Ord, Show)

{-# INLINE split #-}
split :: Measured v a => (v -> Bool) -> SplayTree v a -> SplitResult v a
split = go mempty
  where
    go _ _ Leaf = Outside
    go v f (Fork l a r _)
      | f vl = case go v f l of
        Outside -> Outside
        Inside l' a' m -> Inside l' a' $ fork m a r
      | f vla = Inside l a r
      | otherwise = case go vla f r of
        Outside -> Outside
        Inside m a' r' -> Inside (fork l a m) a' r'
      where
        vl = v <> measure l
        vla = vl <> measure a

-------------------------------------------------------------------------------
-- * Maps

{-# INLINE map #-}
map
  :: (Measured v a, Measured w b)
  => (a -> b)
  -> SplayTree v a
  -> SplayTree w b
map _ Leaf = Leaf
map f (Fork l a r _) = fork (Data.SplayTree.map f l) (f a) (Data.SplayTree.map f r)

{-# INLINE mapWithPos #-}
mapWithPos
  :: (Measured v a, Measured w b)
  => (v -> a -> b)
  -> SplayTree v a
  -> SplayTree w b
mapWithPos f = go mempty
  where
    go _ Leaf = Leaf
    go i (Fork l a r _) = fork (go i l) (f il a) (go ila r)
      where
        il = i <> measure l
        ila = il <> measure a

{-# INLINE mapWithContext #-}
mapWithContext
  :: (Measured v a, Measured w b)
  => (v -> a -> v -> b)
  -> SplayTree v a
  -> SplayTree w b
mapWithContext f t = go mempty t mempty
  where
    go _ Leaf _ = Leaf
    go i (Fork l a r _) j = fork (go i l arj) (f il a rj) (go ila r j)
      where
        ma = measure a
        il = i <> measure l
        ila = il <> ma
        rj = measure r <> j
        arj = ma

-------------------------------------------------------------------------------
-- * Traversals

{-# INLINE traverse #-}
traverse
  :: (Measured v a, Measured w b, Applicative f)
  => (a -> f b)
  -> SplayTree v a
  -> f (SplayTree w b)
traverse _ Leaf = pure Leaf
traverse f (Fork l a r _)
  = fork
  <$> Data.SplayTree.traverse f l
  <*> f a
  <*> Data.SplayTree.traverse f r

{-# INLINE traverseWithPos #-}
traverseWithPos
  :: (Measured v a, Measured w b, Applicative f)
  => (v -> a -> f b)
  -> SplayTree v a
  -> f (SplayTree w b)
traverseWithPos f = go mempty
  where
    go _ Leaf = pure Leaf
    go i (Fork l a r _)
      = fork <$> go i l <*> f il a <*> go ila r
      where
        il = i <> measure l
        ila = il <> measure a

{-# INLINE traverseWithContext #-}
traverseWithContext
  :: (Measured v a, Measured w b, Applicative f)
  => (v -> a -> v -> f b)
  -> SplayTree v a
  -> f (SplayTree w b)
traverseWithContext f t = go mempty t mempty
  where
    go _ Leaf _ = pure Leaf
    go i (Fork l a r _) j
      = fork <$> go i l arj <*> f il a rj <*> go ila r j
      where
        ma = measure a
        il = i <> measure l
        ila = il <> ma
        rj = measure r <> j
        arj = ma
