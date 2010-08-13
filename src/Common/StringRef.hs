-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
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
module Common.StringRef (StringRef, stringRef, toString) where

import Data.Bits
import Data.IORef
import Data.List 
import System.IO.Unsafe
import qualified Data.IntMap as IM

----------------------------------------------------------------
-- StringRef datatype and instance declarations

data StringRef = S !Int 
   deriving (Eq, Ord)

instance Show StringRef where
   show s@(S i) = '#' : show i ++ toString s

----------------------------------------------------------------
-- Hash table

type HashTable = IM.IntMap [String]

tableRef :: IORef HashTable
tableRef = unsafePerformIO (newIORef IM.empty)

----------------------------------------------------------------
-- Conversion to and from strings

stringRef :: String -> StringRef
stringRef s = unsafePerformIO $ do
   let hash = hashString s
   m <- readIORef tableRef
   case IM.insertLookupWithKey (\_ -> combine) hash [s] m of
      (Nothing, new) -> do
         writeIORef tableRef new
         return (S (encodeIndexZero hash))
      (Just old, new) -> 
         case findIndex (==s) old of
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
intErr s = error ("Internal error in Common.StringRef: " ++ s)

----------------------------------------------------------------
-- Testing and debugging 

{-
printTable :: IO ()
printTable = readIORef tableRef >>= print

test1 _ = toString (stringRef "bas") == "bas"
test2 _ = stringRef "bas" == stringRef "bas"
test3 _ = stringRef "bas" /= stringRef "je"
test4 _ = stringRef "arith1.unary_minus" /= stringRef "distribute power"
-}