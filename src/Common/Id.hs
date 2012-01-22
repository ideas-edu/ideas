-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
   ( Id, IsId(..), HasId(..), Identify(..), ( # ), sameId
   , unqualified, qualifiers, qualification
   , describe, description, showId, compareId
   , mempty, isEmptyId
   ) where

import Common.Classes
import Common.Utils (splitsWithElem)
import Common.Utils.StringRef
import Control.Monad
import Data.Char
import Data.List
import Data.Monoid
import Data.Ord
import Test.QuickCheck

---------------------------------------------------------------------
-- Abstract data type and its instances

data Id = Id
   { idList        :: [String]
   , idDescription :: String
   , idRef         :: !StringRef
   }

instance Show Id where
   show = intercalate "." . idList

instance Read Id where
   readsPrec _ =
      return . mapFirst stringId . span isIdChar . dropWhile isSpace

instance Eq Id where
   a == b = idRef a == idRef b

instance Ord Id where
   compare = comparing idRef

instance Monoid Id where
   mempty  = emptyId
   mappend = ( # )

instance Arbitrary Id where
   arbitrary = frequency
      [ (4, do n  <- choose (0, 8)
               xs <- replicateM n (elements ['a' .. 'z'])
               return $ newId xs)
      , (1, liftM2 mappend arbitrary arbitrary)
      ]

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
   newId = const emptyId

instance (IsId a, IsId b) => IsId (a, b) where
   newId (a, b) = newId a # newId b

instance (IsId a, IsId b, IsId c) => IsId (a, b, c) where
   newId (a, b, c) = newId a # newId b # newId c

instance IsId a => IsId (Maybe a) where
   newId = maybe emptyId newId

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
   changeId f = biMap (changeId f) (changeId f)

class HasId a => Identify a where
   (@>) :: IsId n => n -> a -> a

---------------------------------------------------------------------
-- Private constructors

appendId :: Id -> Id -> Id
appendId a b
   | null (idList a) = b
   | null (idList b) = a
   | otherwise       = Id (idList a ++ idList b) "" ref
 where
   ref = stringRef (show a ++ "." ++ show b)

-- Only allow alphanum and '-' ('.' has a special meaning)
stringId :: String -> Id
stringId txt = Id (make s) "" (stringRef s)
 where
   s    = norm txt
   make = filter (not . null) . splitsWithElem '.'
   norm = filter isIdChar . map toLower

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c `elem` ".-_"

emptyId :: Id
emptyId = Id [] "" (stringRef "")

---------------------------------------------------------------------
-- Additional functionality (overloaded)

infixr 8 #

( # ) :: (IsId a, IsId b) => a -> b -> Id
a # b = appendId (newId a) (newId b)

sameId :: (IsId a, IsId b) => a -> b -> Bool
sameId a b = newId a == newId b

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
qualification = intercalate "." . qualifiers

description :: HasId a => a -> String
description = idDescription . getId

showId :: HasId a => a -> String
showId = show . getId

compareId :: HasId a => a -> a -> Ordering
compareId = comparing showId

isEmptyId :: Id -> Bool
isEmptyId = (== emptyId)

describe :: HasId a => String -> a -> a
describe = changeId . describeId
 where
   describeId s a
      | null (idDescription a) =
           a {idDescription = s}
      | otherwise =
           a {idDescription = s ++ " " ++ idDescription a}