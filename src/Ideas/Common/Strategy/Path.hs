-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A path encodes a location (sub-part) of a strategy by:
-- * maintaining a list of directions (go left/go right) for each choice;
-- * the depth (number of steps)
-- Use Show/Read type classes for serialization/deserialization
--
-----------------------------------------------------------------------------
module Ideas.Common.Strategy.Path 
   ( -- * datatype and constructor
     Path, emptyPath
     -- * extending a path
   , toLeft, toRight, tick
     -- * following a path
   , leftOrRight, untick
   ) where

import Ideas.Common.Classes
import Data.Sequence (Seq, empty, (|>), viewl, ViewL(..), fromList)
import Data.Foldable (toList)

data Path = Path !Int (Seq Bool) -- depth, choices
   deriving Eq
   
instance Show Path where
   show = show . intList

instance Read Path where
   readsPrec _ = map (mapFirst fromIntList) . readList

emptyPath :: Path
emptyPath = Path 0 empty

toLeft, toRight, tick :: Path -> Path
toLeft  (Path n bs) = Path (n+1) (bs |> True)
toRight (Path n bs) = Path (n+1) (bs |> False)
tick    (Path n bs) = Path (n+1) bs

-- |Following a path without going left or right (counterpart of 'tick')
untick :: Monad m => Path -> m Path
untick (Path n bs)
   | n > 0     = return (Path (n-1) bs)
   | otherwise = fail "untick: invalid path"

leftOrRight :: Monad m => Path -> m (Either Path Path)
leftOrRight (Path n bs) =
   case viewl bs of
      b :< cs | n > 0 && b -> return (Left (Path (n-1) cs))
              | n > 0      -> return (Right (Path (n-1) cs))
      _ -> fail "untick: invalid path"

-- local helpers
intList :: Path -> [Int]
intList (Path n bs)
   | n == 0    = []
   | otherwise = n : map (\b -> if b then 0 else 1) (toList bs)

fromIntList :: [Int] -> Path
fromIntList []     = emptyPath
fromIntList (n:is) = Path n (fromList (map (==0) is))