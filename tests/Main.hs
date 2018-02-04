module Main where

import Data.Semigroup
import qualified Data.Text as Text
import qualified Data.Text.Unsafe as Unsafe
import Test.QuickCheck.Function as QC
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Rope.UTF16 as Rope
import qualified Data.Rope.UTF16.Internal.Text as Rope

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testProperty "toText . fromText = id" $ \s -> do
    let t = Text.pack s
    Rope.toText (Rope.fromText t) == t

  , testProperty "append matches Text" $ \s1 s2 -> do
    let t1 = Text.pack s1
        t2 = Text.pack s2
    Rope.toText (Rope.fromText t1 <> Rope.fromText t2) == t1 <> t2

  , testProperty "length is UTF-16 code points" $ \s -> do
    let t = Text.pack s
    Rope.length (Rope.fromText t) == Unsafe.lengthWord16 t

  , testProperty "splitAt matches Text" $ \s i -> do
    let t = Text.pack s
        (r1, r2) = Rope.splitAt i $ Rope.fromText t
    (Rope.toText r1, Rope.toText r2) == Rope.split16At i t

  , testProperty "take matches Text" $ \s i -> do
    let t = Text.pack s
    Rope.toText (Rope.take i $ Rope.fromText t) == Rope.take16 i t

  , testProperty "drop matches Text" $ \s i -> do
    let t = Text.pack s
    Rope.toText (Rope.drop i $ Rope.fromText t) == Rope.drop16 i t

  , testProperty "rowColumnCodePoints first line" $ \s i -> do
    let t = Text.pack $ takeWhile (/= '\n') s
    Rope.clamp16 i t == Rope.rowColumnCodePoints (Rope.RowColumn 0 i) (Rope.fromText t)

  , testProperty "rowColumnCodePoints subsequent lines" $ \s (NonNegative newlines) (NonNegative i) -> do
    let t = Text.pack $ replicate newlines '\n' ++ takeWhile (/= '\n') s
    Rope.clamp16 (newlines + i) t == Rope.rowColumnCodePoints (Rope.RowColumn newlines i) (Rope.fromText t)

  , testProperty "span matches Text" $ \s p -> do
    let t = Text.pack s
        f = QC.applyFun p
        (r1, r2) = Rope.span f $ Rope.fromText t
    (Rope.toText r1, Rope.toText r2) == Text.span f t

  , testProperty "break matches Text" $ \s p -> do
    let t = Text.pack s
        f = QC.applyFun p
        (r1, r2) = Rope.break f $ Rope.fromText t
    (Rope.toText r1, Rope.toText r2) == Text.break f t

  , testProperty "takeWhile matches Text" $ \s p -> do
    let t = Text.pack s
        f = QC.applyFun p
    Rope.toText (Rope.takeWhile f $ Rope.fromText t) == Text.takeWhile f t

  , testProperty "dropWhile matches Text" $ \s p -> do
    let t = Text.pack s
        f = QC.applyFun p
    Rope.toText (Rope.dropWhile f $ Rope.fromText t) == Text.dropWhile f t
  ]
