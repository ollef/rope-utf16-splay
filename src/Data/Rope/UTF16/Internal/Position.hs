module Data.Rope.UTF16.Internal.Position where

import Data.Semigroup

data RowColumn = RowColumn
  { row :: !Int -- ^ Number of newlines before this position
  , column :: !Int -- ^ Number of UTF-16 code units since last newline or start of string
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
  { codeUnits :: !Int -- ^ UTF-16 code units
  , rowColumn :: !RowColumn
  } deriving (Eq, Ord, Show)

instance Semigroup Position where
  Position cu rc <> Position cu' rc'
    = Position (cu + cu') (rc <> rc')

instance Monoid Position where
  mempty = Position 0 mempty
  mappend = (<>)
