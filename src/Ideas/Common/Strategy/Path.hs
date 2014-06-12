-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
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
--  $Id$

module Ideas.Common.Strategy.Path
   ( Path, emptyPath
   , fromIntList, intList
   ) where

import Ideas.Common.Classes

newtype Path = Path [Int]
   deriving Eq

instance Show Path where
   show = show . intList

instance Read Path where
   readsPrec _ = map (mapFirst fromIntList) . readList

emptyPath :: Path
emptyPath = Path []

-- local helpers
intList :: Path -> [Int]
intList (Path ns) = ns

fromIntList :: [Int] -> Path
fromIntList = Path