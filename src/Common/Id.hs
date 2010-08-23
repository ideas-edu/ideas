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
module Common.Id 
   ( Id, HasId(..), newId, newIdM
   , unqualified, qualifiers, qualification
   , describe, description, showId, compareId
   ) where

import Data.List
import Control.Monad.Error ()
import Common.StringRef

data Id = Id 
   { idName        :: String
   , idQualifiers  :: [String]
   , idDescription :: String
   , idRef         :: StringRef
   }
   
instance Show Id where
   show a = foldr (\x y -> x ++ "." ++ y) (idName a) (idQualifiers a)
   -- show a = toString (idRef a)

instance Eq Id where
   a == b = idRef a == idRef b

instance Ord Id where 
   a `compare` b = idRef a `compare` idRef b
   
unqualified :: HasId a => a -> String
unqualified = idName . getId

qualifiers :: HasId a => a -> [String]
qualifiers = idQualifiers . getId

qualification :: HasId a => a -> String
qualification = concat . intersperse "." . qualifiers

description :: HasId a => a -> String 
description = idDescription . getId

showId :: HasId a => a -> String
showId = show . getId

compareId :: HasId a => a -> a -> Ordering
compareId a b = showId a `compare` showId b

-- For now, all characters are allowed. To do: make this 
-- more strict, e.g. by removing spaces.
-- Proposal: allow alphanum + '-'
readId :: Monad m => String -> m ([String], String)
readId a = f [] a
 where
   f acc s =
      case break (== '.') s of
         (xs, _:ys) | not (null xs) -> f (xs:acc) ys
         (xs, [])   | not (null xs) -> return (reverse acc, xs)
         _ -> fail $ "Invalid id: " ++ a

newId :: String -> Id
newId = either error id . newIdM

newIdM :: Monad m => String -> m Id
newIdM a = do 
   (qs, n) <- readId a
   return (Id n qs "" (stringRef a))

class HasId a where
   getId    :: a -> Id
   changeId :: (Id -> Id) -> a -> a
 
instance HasId Id where
   getId    = id
   changeId = id

instance (HasId a, HasId b) => HasId (Either a b) where
   getId      = either getId getId
   changeId f = either (Left . changeId f) (Right . changeId f)

describe :: HasId a => String -> a -> a
describe = changeId . describeId

describeId :: String -> Id -> Id
describeId s a
   | null (idDescription a) = 
        a {idDescription = s}
   | otherwise =
        a {idDescription = s ++ " " ++ idDescription a}