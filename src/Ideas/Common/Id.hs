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
-- Many entities of the Ideas framework carry an 'Id' for identification.
-- Identifiers have a hierarchical structure of an arbitrary depth (e.g.
-- @algebra.equation@ or @a.b.c@). Valid symbols for identifiers are the
-- alpha-numerical characters, together with @-@ and @_@. Each identifier
-- carries a description and a hash value for fast comparison.
--
-- Functionality for identifiers is provided by means of three type classes:
--
-- * Type class 'IsId' for constructing identifiers
--
-- * Type class 'HasId' for accessing (and changing) the identifier of an
--   entity. Instances of this type class must always have exactly one
--   identifier (although this identifier can be empty).
--
-- * Type class 'Identify' for labeling entities with an identifier. Instances
-- of this type class typically allow labels to appear at multiple locations
-- within their structure.
--
-- The 'Id' datatype implements the Monoid interface.
--
-----------------------------------------------------------------------------

module Ideas.Common.Id
   ( -- * Constructing identifiers
     Id, IsId(..), ( # )
     -- * Accessing (and changing) identifiers
   , HasId(..), unqualified, qualifiers, qualification
   , describe, description, showId, compareId
     -- * Labeling with identifiers
   , Identify(..)
   ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Data.Semigroup as Sem
import Ideas.Common.Classes
import Ideas.Text.HTML
import Ideas.Utils.Prelude (splitsWithElem)
import Ideas.Utils.StringRef
import Test.QuickCheck

---------------------------------------------------------------------
-- Abstract data type and its instances

-- | Abstract data type for identifiers with a hierarchical name, carrying
-- a description. The data type provides a fast comparison implementation.
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

instance Sem.Semigroup Id where
   (<>) = ( # )

instance Monoid Id where
   mempty  = emptyId
   mappend = (<>)

instance Arbitrary Id where
   arbitrary = frequency
      [ (4, do n  <- choose (0, 8)
               xs <- replicateM n (elements ['a' .. 'z'])
               return $ newId xs)
      , (1, liftM2 mappend arbitrary arbitrary)
      ]

instance ToHTML Id where
   toHTML = text

-- | Type class 'IsId' for constructing identifiers. Examples are
-- @newId \"algebra.equation\"@, @newId (\"a\", \"b\", \"c\")@, and @newId ()@
-- for the empty identifier.
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

infixr 8 #

-- | Appends two identifiers. Both parameters are overloaded.
( # ) :: (IsId a, IsId b) => a -> b -> Id
a # b = appendId (newId a) (newId b)

-----------------------------------------------------
-- Type class for structures containing an identifier

-- | Type class for labeling entities with an identifier
class HasId a => Identify a where
   (@>) :: IsId n => n -> a -> a

-- | Type classfor accessing (and changing) the identifier of an entity.
class HasId a where
   getId    :: a -> Id
   changeId :: (Id -> Id) -> a -> a

instance HasId Id where
   getId    = id
   changeId = id

instance (HasId a, HasId b) => HasId (Either a b) where
   getId      = either getId getId
   changeId f = biMap (changeId f) (changeId f)

---------------------------------------------------------------------
-- Private constructors

appendId :: Id -> Id -> Id
appendId a b
   | null (idList a) = b
   | null (idList b) = a
   | otherwise       = Id (idList a ++ idList b) "" ref
 where
   ref = stringRef (show a ++ "." ++ show b)

-- Only allow alphanum and '-' and '_' ('.' has a special meaning)
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

-- | Get the unqualified part of the identifier (i.e., last string).
unqualified :: HasId a => a -> String
unqualified a
   | null xs   = ""
   | otherwise = last xs
 where
   xs = idList (getId a)

-- | Get the list of qualifiers of the identifier (i.e., everything but the
-- last string).
qualifiers :: HasId a => a -> [String]
qualifiers a
   | null xs   = []
   | otherwise = init xs
 where
   xs = idList (getId a)

-- | Get the qualified part of the identifier. If the identifier consists of
-- more than one part, the parts are separated by a period (@'.'@).
qualification :: HasId a => a -> String
qualification = intercalate "." . qualifiers

-- | Get the current description.
description :: HasId a => a -> String
description = idDescription . getId

-- | Give a description for the current entity. If there already is a
-- description, both strings are combined.
describe :: HasId a => String -> a -> a
describe = changeId . describeId
 where
   describeId s a
      | null (idDescription a) =
           a {idDescription = s}
      | otherwise =
           a {idDescription = s ++ " " ++ idDescription a}

-- | Show the identifier.
showId :: HasId a => a -> String
showId = show . getId

-- | Compare two identifiers based on their names. Use @compare@ for a fast
-- ordering based on hash values.
compareId :: HasId a => a -> a -> Ordering
compareId = comparing showId