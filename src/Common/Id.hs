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
   ( Id, IsId(..), HasId(..), (#)
   , unqualified, qualifiers, qualification
   , describe, description, showId, compareId
   ) where

import Data.Char
import Data.List
import Data.Monoid
import Common.StringRef
import Common.Utils (splitsWithElem)


---------------------------------------------------------------------
-- Abstract data type and its instances

data Id = Id 
   { idList        :: [String]
   , idDescription :: String
   , idRef         :: StringRef
   }
   
instance Show Id where
   show = concat . intersperse "." . idList

instance Eq Id where
   a == b = idRef a == idRef b

instance Ord Id where 
   a `compare` b = idRef a `compare` idRef b

instance Monoid Id where
   mempty  = stringId ""
   mappend = (#)

---------------------------------------------------------------------
-- Type class for constructing identifiers

class IsId a where
   newId    :: a   -> Id
   concatId :: [a] -> Id -- for String instance
   -- default definition
   concatId = mconcat . map newId

instance IsId Id where
   newId = id

instance IsId Char where
   newId c  = stringId [c]
   concatId = stringId

instance IsId a => IsId [a] where
   newId    = concatId
   concatId = mconcat . map newId

instance IsId () where
   newId = const mempty

instance (IsId a, IsId b) => IsId (a, b) where
   newId (a, b) = newId a # newId b
   
instance (IsId a, IsId b, IsId c) => IsId (a, b, c) where
   newId (a, b, c) = newId a # newId b # newId c
   
instance IsId a => IsId (Maybe a) where
   newId = maybe mempty newId
   
instance (IsId a, IsId b) => IsId (Either a b) where
   newId = either newId newId

-----------------------------------------------------
-- Type class for structures containing an identifier
   
class HasId a where
   getId    :: a -> Id
   changeId :: (Id -> Id) -> a -> a
 
instance HasId Id where
   getId    = id
   changeId = id

instance (HasId a, HasId b) => HasId (Either a b) where
   getId      = either getId getId
   changeId f = either (Left . changeId f) (Right . changeId f)
   
---------------------------------------------------------------------
-- Private constructors

appendId :: Id -> Id -> Id
appendId a b
   | null (idList a) = b
   | null (idList b) = a
   | otherwise       = Id (idList a ++ idList b) "" ref
 where
   ref = stringRef (show a ++ "." ++ show b)

-- For now, all characters are allowed. To do: make this 
-- more strict, e.g. by removing spaces.
-- Proposal: allow alphanum + '-'
stringId :: String -> Id
stringId txt = Id (make s) "" (stringRef s)
 where
   s    = normalize txt
   make = filter (not . null) . splitsWithElem '.'
   normalize = map toLower

---------------------------------------------------------------------
-- Additional functionality (overloaded)
   
infixr 8 #

(#) :: (IsId a, IsId b) => a -> b -> Id
a # b = appendId (newId a) (newId b)
   
unqualified :: HasId a => a -> String
unqualified a
   | null xs   = ""
   | otherwise = last xs
 where
   xs = idList (getId a)

qualifiers :: HasId a => a -> [String]
qualifiers a
   | null xs   = []
   | otherwise = init xs
 where
   xs = idList (getId a)

qualification :: HasId a => a -> String
qualification = concat . intersperse "." . qualifiers

description :: HasId a => a -> String 
description = idDescription . getId

showId :: HasId a => a -> String
showId = show . getId

compareId :: HasId a => a -> a -> Ordering
compareId a b = showId a `compare` showId b

describe :: HasId a => String -> a -> a
describe = changeId . describeId
 where
   describeId s a
      | null (idDescription a) = 
           a {idDescription = s}
      | otherwise =
           a {idDescription = s ++ " " ++ idDescription a}