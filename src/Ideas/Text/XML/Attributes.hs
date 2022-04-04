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
-----------------------------------------------------------------------------

module Ideas.Text.XML.Attributes 
   ( Attributes, HasAttributes(..)
   , attribute
   , noAttributes, lookupAttribute
   , foldAttributes
   ) where

import Ideas.Text.XML.Document (Name)
import qualified Data.Map as M

newtype Attributes = A { attributeMap :: M.Map Name String }
 deriving Eq

data Attribute = Name := String
 deriving Eq

instance Semigroup Attributes where
   A xs <> A ys = A $ M.unionWith (\x y -> x ++ " " ++ y) xs ys

instance Monoid Attributes where
   mempty = A M.empty

class HasAttributes a where
   getAttributes    :: a -> Attributes
   setAttributes    :: Attributes -> a -> a
   changeAttributes :: (Attributes -> Attributes) -> a -> a
   updateAttributes :: a -> (Attributes, Attributes -> a)
   
   {-# MINIMAL (getAttributes, changeAttributes) | updateAttributes #-}

   -- default definitions
   getAttributes      = fst . updateAttributes
   setAttributes      = changeAttributes . const
   changeAttributes f = (\(c, g) -> g (f c)) . updateAttributes
   updateAttributes a = (getAttributes a, (`setAttributes` a))

instance HasAttributes Attributes where
   updateAttributes a = (a, id)

attribute :: Name -> String -> Attributes
attribute n s = A $ M.singleton n s

lookupAttribute :: HasAttributes a => Name -> a -> Maybe String
lookupAttribute n = M.lookup n . attributeMap . getAttributes

noAttributes :: HasAttributes a => a -> Bool
noAttributes = null . attributeMap . getAttributes

foldAttributes :: Monoid a => (Name -> String -> a) -> Attributes -> a
foldAttributes fa = M.foldrWithKey f mempty . attributeMap
 where
   f n s = (fa n s <>)