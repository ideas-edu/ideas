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
   { idName        :: String
   , idQualifiers  :: [String]
   , idType        :: IdType
   , idDescription :: String
   }
   
instance Show Id where
   show a = concat (intersperse "." (idQualifiers a ++ [idName a]))

instance Eq Id where
   a == b = a `compare` b == EQ

instance Ord Id where 
   a `compare` b = f a `compare` f b
    where f x = (idQualifiers x, idName x)
   
data IdType = IdExercise | IdRule
   deriving (Eq, Ord)

instance Show IdType where
   show IdExercise = "Exercise"
   show IdRule     = "Rule"

identifier :: HasId a => a -> String
identifier = idName . getId

qualifiers :: HasId a => a -> [String]
qualifiers = idQualifiers . getId

qualification :: HasId a => a -> String
qualification = concat . intersperse "." . qualifiers

unqualified :: HasId a => a -> String
unqualified = idName . getId

description :: HasId a => a -> String 
description = idDescription . getId

showId :: HasId a => a -> String
showId = show . getId

newId :: IdType -> String -> Id
newId tp s = Id s [] tp ""

newQId :: IdType -> String -> String -> Id
newQId tp q s = Id s [q] tp ""

class HasId a where
   getId    :: a -> Id
   changeId :: (Id -> Id) -> a -> a
 
instance HasId Id where
   getId    = id
   changeId = id

describe :: HasId a => String -> a -> a
describe = changeId . describeId

describeId :: String -> Id -> Id
describeId s a
   | null (idDescription a) = 
        a { idDescription = s }
   | otherwise =
        a { idDescription = s ++ " " ++ idDescription a }