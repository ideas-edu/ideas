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
-- Identification of entities
--
-----------------------------------------------------------------------------
module Common.Id where

import Data.List

data Id = Id 
   { quantifiers :: [String]
   , identifier  :: String
   , idType      :: IdType
   , idDescription :: String
   }
   
instance Show Id where
   show a = concat (intersperse "." (quantifiers a ++ [identifier a]))

instance Eq Id where
   a == b = a `compare` b == EQ

instance Ord Id where 
   a `compare` b = f a `compare` f b
    where f x = (quantifiers x, identifier x)
   
data IdType = IdExercise 
   deriving (Eq, Ord)

instance Show IdType where
   show IdExercise = "Exercise"

newId :: String -> Id
newId s = Id [] s IdExercise ""

newQId :: String -> String -> Id
newQId q s = Id [q] s IdExercise ""

class HasId a where
   getId    :: a -> Id
   changeId :: (Id -> Id) -> a -> a
 
instance HasId Id where
   getId    = id
   changeId = id

describe :: HasId a => String -> a -> a
describe = changeId . describeId

description :: HasId a => a -> String 
description = idDescription . getId

describeId :: String -> Id -> Id
describeId s a
   | null (idDescription a) = 
        a { idDescription = s }
   | otherwise =
        a { idDescription = s ++ " " ++ idDescription a }