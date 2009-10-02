{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Common.Rewriting.MetaVar where

import Common.Uniplate
import Common.Utils (readInt)
import Data.Char (ord, isDigit)
import Data.List
import qualified Data.IntSet as IS


-----------------------------------------------------------
--- Meta variables

-- | Type class for creating meta-variables
class MetaVar a where
    metaVar   :: Int -> a
    isMetaVar :: a -> Maybe Int

instance MetaVar String where
   isMetaVar  ('_':xs) = readInt xs
   isMetaVar _         = Nothing
   metaVar n = '_' : show n
   
-- | Produces an infinite list of meta-variables
metaVars :: MetaVar a => [a]
metaVars = map metaVar [0..]

-- | Collect all meta-variables
getMetaVars :: (MetaVar a, Uniplate a) => a -> IS.IntSet
getMetaVars a = getMetaVarsList [a]

-- | Collect all meta-variables in the list
getMetaVarsList :: (MetaVar a, Uniplate a) => [a] -> IS.IntSet
getMetaVarsList xs = IS.fromList [ i | x <- xs, a <- universe x, Just i <- [isMetaVar a] ]

-- | Checks whether the meta-variable is used in a term
hasMetaVar :: (MetaVar a, Uniplate a) => Int -> a -> Bool
hasMetaVar i = IS.member i . getMetaVars

-- | Checks whether the meta-variable is used in one of the elements in the list
hasMetaVarList :: (MetaVar a, Uniplate a) => Int -> [a] -> Bool
hasMetaVarList i = IS.member i . getMetaVarsList

-- | Checks whether a value has no variables
noMetaVars :: (Uniplate a, MetaVar a) => a -> Bool
noMetaVars = IS.null . getMetaVars  

-- | Determine what the next unused meta-varable is
nextMetaVar :: (Uniplate a, MetaVar a) => a -> Int
nextMetaVar a = nextMetaVarOfList [a]

-- | Determine what the next meta-variable is that is not used in
-- an element of the list
nextMetaVarOfList :: (Uniplate a, MetaVar a) => [a] -> Int
nextMetaVarOfList xs
   | IS.null s = 0
   | otherwise = 1 + IS.findMax s
 where
   s = getMetaVarsList xs

-- | Rename the meta-variables 
renameMetaVars :: (MetaVar a, Uniplate a) => (Int -> Int) -> a -> a
renameMetaVars f a =
   case isMetaVar a of
      Just i  -> metaVar (f i)
      Nothing -> g $ map (renameMetaVars f) cs
 where 
   (cs, g) = uniplate a