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
-- Substitutions
--
-----------------------------------------------------------------------------
module Common.Rewriting.Substitution 
   ( Substitution, emptySubst, singletonSubst, listToSubst, (@@), (@@@)
   , lookupVar, dom, removeDom, ran, (|->)
   ) where

import Common.Uniplate
import Common.Rewriting.MetaVar
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Maybe

-----------------------------------------------------------
--- Substitution

-- | Abstract data type for substitutions
newtype Substitution a = S { unS :: IM.IntMap a }
   
infixr 4 |->
infixr 5 @@, @@@

instance Show a => Show (Substitution a) where
   show = show . unS

-- | Returns the empty substitution
emptySubst :: (Uniplate a, MetaVar a) => Substitution a
emptySubst = S IM.empty

-- | Returns a singleton substitution
singletonSubst :: (MetaVar a, Uniplate a) => Int -> a -> Substitution a
singletonSubst i a = S (IM.singleton i a)

-- | Turns a list into a substitution
listToSubst :: (Uniplate a, MetaVar a) => [(Int, a)] -> Substitution a
listToSubst = S . IM.fromListWith (error "Substitution: keys are not unique")

-- | Combines two substitutions. The left-hand side substitution is first applied to
-- the co-domain of the right-hand side substitution
(@@) :: (Uniplate a, MetaVar a) => Substitution a -> Substitution a -> Substitution a
S a @@ S b = S $ a `IM.union` IM.map (S a |->) b

-- | Combines two substitutions with disjoint domains. If the domains are not disjoint,
-- an error is reported
(@@@) :: (Uniplate a, MetaVar a) => Substitution a -> Substitution a -> Substitution a
S a @@@ S b = S (IM.unionWith err a b)
 where err _ _ = error "Unification.(@@@): domains of substitutions are not disjoint"

-- | Lookups a variable in a substitution. Nothing indicates that the variable is
-- not in the domain of the substitution
lookupVar :: Int -> Substitution a -> Maybe a
lookupVar s = IM.lookup s . unS

-- | Returns the domain of a substitution (as a list)
dom :: Substitution a -> IS.IntSet
dom = IM.keysSet . unS

-- | Removes variables from the domain of a substitution
removeDom :: IS.IntSet -> Substitution a -> Substitution a
removeDom s (S a) = S (IM.filterWithKey (\k _ -> IS.member k s) a)

ran :: Substitution a -> [a]
ran = IM.elems . unS

-- | Apply the substitution
(|->) :: (MetaVar a, Uniplate a) => Substitution a -> a -> a
s |-> e = 
   case isMetaVar e of
      Just i  -> fromMaybe e (lookupVar i s)
      Nothing -> let (cs, f) = uniplate e
                 in f (map (s |->) cs)