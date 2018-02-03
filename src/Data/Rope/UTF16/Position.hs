module Data.Rope.UTF16.Position where

import Data.Semigroup

data Visual = Visual
  { row :: !Int -- ^ Number of newlines before this position
  , column :: !Int -- ^ Number of code points since last newline or start of string
  } deriving (Eq, Ord, Show)

instance Semigroup Visual where
  Visual 0 c1 <> Visual 0 c2 = Visual 0 $ c1 + c2
  Visual 0 _ <> v2 = v2
  Visual r1 c1 <> Visual 0 c2 = Visual r1 $ c1 + c2
  Visual r1 _ <> Visual r2 c2 = Visual (r1 + r2) c2

instance Monoid Visual where
  mempty = Visual 0 0
  mappend = (<>)

data Position = Position
  { codePoints :: !Int
  , visual :: !Visual
  } deriving (Eq, Ord, Show)

lineStart :: Position -> Position
lineStart (Position cp (Visual r c)) = Position (cp - c) $ Visual r 0

instance Semigroup Position where
  Position cp1 v1 <> Position cp2 v2
    = Position (cp1 + cp2) (v1 <> v2)

instance Monoid Position where
  mempty = Position 0 mempty
  mappend = (<>)
