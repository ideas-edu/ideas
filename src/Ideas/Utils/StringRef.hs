-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- References to Strings, proving a fast comparison implementation (Eq and
-- Ord) that uses a hash function. Code is based on Daan Leijen's Lazy
-- Virutal Machine (LVM) identifiers.
--
-----------------------------------------------------------------------------

module Ideas.Utils.StringRef
   ( StringRef, stringRef, toString, tableStatus
   ) where

import Data.Bits
import Data.IORef
import Data.List
import System.IO.Unsafe
import qualified Data.IntMap as IM

----------------------------------------------------------------
-- StringRef datatype and instance declarations

data StringRef = S !Int
   deriving (Eq, Ord)

----------------------------------------------------------------
-- Hash table

type HashTable = IM.IntMap [String]

{-# NOINLINE tableRef #-}
tableRef :: IORef HashTable
tableRef = unsafePerformIO (newIORef IM.empty)

----------------------------------------------------------------
-- Conversion to and from strings

stringRef :: String -> StringRef
stringRef s = unsafePerformIO $ do
   let hash = hashString s
   m <- readIORef tableRef
   case IM.insertLookupWithKey (const combine) hash [s] m of
      (Nothing, new) -> do
         writeIORef tableRef new
         return (S (encodeIndexZero hash))
      (Just old, new) ->
         case elemIndex s old of
            Just index ->
               return (S (encode hash index))
            Nothing -> do
               let index = length old
               writeIORef tableRef new
               return (S (encode hash index))

toString :: StringRef -> String
toString (S i) = unsafePerformIO $ do
   m <- readIORef tableRef
   case IM.lookup (extractHash i) m of
      Just xs -> return (atIndex (extractIndex i) xs)
      Nothing -> intErr "id not found"

----------------------------------------------------------------
-- Bit encoding

encode :: Int -> Int -> Int
encode hash index = hash + index `shiftL` 12

encodeIndexZero :: Int -> Int
encodeIndexZero hash = hash

extractHash :: Int -> Int
extractHash i = i `mod` 4096

extractIndex :: Int -> Int
extractIndex i = i `shiftR` 12

----------------------------------------------------------------
-- Hash function

-- simple hash function that performs quite good in practice
hashString :: String -> Int
hashString s = (f s `mod` prime) `mod` maxHash
 where
   f        = foldl' next 0   -- ' strict fold
   next n c = n*65599 + fromEnum c
   prime    = 32537 --65599   -- require: prime < maxHash

maxHash :: Int
maxHash = 0xFFF -- 12 bits

----------------------------------------------------------------
-- Utility functions

atIndex :: Int -> [a] -> a
atIndex 0 (x:_)  = x
atIndex i (_:xs) = atIndex (i-1) xs
atIndex _ _      = intErr "corrupt symbol table"

combine :: Eq a => [a] -> [a] -> [a]
combine [a] = rec
 where
   rec [] = [a]
   rec this@(x:xs)
      | a == x    = this
      | otherwise = x:rec xs
combine _ = intErr "combine"

intErr :: String -> a
intErr s = error ("Internal error in Ideas.Common.StringRef: " ++ s)

----------------------------------------------------------------
-- Testing and debugging

tableStatus :: IO String
tableStatus = readIORef tableRef >>= \m ->
   let xs = map f (IM.assocs m)
       f (i, ys) = '#' : show i ++ ": " ++ intercalate ", " (map g (frequency ys)) ++
                   "  [total = " ++ show (length ys) ++ "]"
       g (a, n)  | n == 1    = show a
                 | otherwise = show a ++ " (" ++ show n ++ ")"
   in return $ unlines xs

frequency :: Eq a => [a] -> [(a, Int)]
frequency [] = []
frequency (x:xs) =
   let (ys, zs) = partition (==x) xs
   in (x, 1+length ys) : frequency zs