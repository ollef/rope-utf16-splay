module Data.Rope.UTF16.Position where

import Data.Semigroup

data RowColumn = RowColumn
  { row :: !Int -- ^ Number of newlines before this position
  , column :: !Int -- ^ Number of code points since last newline or start of string
  } deriving (Eq, Ord, Show)

instance Semigroup RowColumn where
  RowColumn 0 c1 <> RowColumn 0 c2 = RowColumn 0 $ c1 + c2
  RowColumn 0 _ <> v2 = v2
  RowColumn r1 c1 <> RowColumn 0 c2 = RowColumn r1 $ c1 + c2
  RowColumn r1 _ <> RowColumn r2 c2 = RowColumn (r1 + r2) c2

instance Monoid RowColumn where
  mempty = RowColumn 0 0
  mappend = (<>)

data Position = Position
  { codePoints :: !Int
  , rowColumn :: !RowColumn
  } deriving (Eq, Ord, Show)

lineStart :: Position -> Position
lineStart (Position cp (RowColumn r c)) = Position (cp - c) $ RowColumn r 0

instance Semigroup Position where
  Position cp1 v1 <> Position cp2 v2
    = Position (cp1 + cp2) (v1 <> v2)

instance Monoid Position where
  mempty = Position 0 mempty
  mappend = (<>)
