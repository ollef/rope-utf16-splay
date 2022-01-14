{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
-- | Helpers for working with 'Text' in UTF-16 code units
module Data.Rope.UTF16.Internal.Text where

import Data.Text(Text)
import qualified Data.Text.Array as Array
import Data.Bits
import qualified Data.Text.Internal as Text
import qualified Data.Text.Unsafe as Unsafe
import qualified Data.Text as Text
import Data.Char

lengthWord16 :: Text -> Int
lengthWord16 = Text.foldl' (\n c -> n + utf16Length c) 0

utf16Length :: Char -> Int
utf16Length c
  | ord c < 0x10000 = 1
  | otherwise = 2

index8To16 :: Int -> Text -> Int
index8To16 index8 t = go 0 0
  where
    index8' = min (Unsafe.lengthWord8 t) index8
    go !i8 !i16
      | i8 >= index8' = i16
      | otherwise = do
        let Unsafe.Iter c delta = Unsafe.iter t i8
        go (i8 + delta) (i16 + utf16Length c)

index16To8 :: Int -> Text -> Int
index16To8 index16 t = go 0 0
  where
    length8 = Unsafe.lengthWord8 t
    go !i8 !i16
      | i8 >= length8 = i8
      | i16 >= index16 = i8
      | otherwise = do
        let Unsafe.Iter c delta = Unsafe.iter t i8
        go (i8 + delta) (i16 + utf16Length c)

take16 :: Int -> Text -> Text
take16 i16 t = Unsafe.takeWord8 (index16To8 i16 t) t

drop16 :: Int -> Text -> Text
drop16 i16 t = Unsafe.dropWord8 (index16To8 i16 t) t

split16At :: Int -> Text -> (Text, Text)
split16At i16 t = split8At (index16To8 i16 t) t

split8At :: Int -> Text -> (Text, Text)
split8At i8 t = (Unsafe.takeWord8 i8 t, Unsafe.dropWord8 i8 t)

clamp8 :: Int -> Text -> Int
clamp8 i t@(Text.Text arr off _len)
  | i <= 0 = 0
  | i >= len = len
  | isFirstCodeUnit = i
  | otherwise = clamp8 (i - 1) t
  where
    cu = Array.unsafeIndex arr (off + i)
    len = Unsafe.lengthWord8 t
    isFirstCodeUnit = cu .&. 0b1100_0000 /= 0b1000_0000

chunks8Of :: Int -> Text -> [Text]
chunks8Of n t
  | len == 0 = []
  | len <= n = [t]
  | otherwise = pre : chunks8Of n post
  where
    (pre, post) = split8At (clamp8 n t) t
    len = Unsafe.lengthWord8 t
