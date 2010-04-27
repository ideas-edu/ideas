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
-- Substitutions on terms
--
-----------------------------------------------------------------------------
module Common.Rewriting.Substitution 
   ( Substitution, emptySubst, singletonSubst, dom
   , (@@), (@@@), (|->), listToSubst
   ) where

import Common.Uniplate
import Common.Rewriting.Term hiding (S)
import qualified Data.IntMap as IM
import Data.Maybe

-----------------------------------------------------------
--- Substitution

-- | Abstract data type for substitutions
newtype Substitution = S { unS :: IM.IntMap Term }
   
infixr 4 |->
infixr 5 @@, @@@

instance Show Substitution where
   show = show . unS

-- | Returns the empty substitution
emptySubst :: Substitution
emptySubst = S IM.empty

-- | Returns a singleton substitution
singletonSubst :: Int -> Term -> Substitution
singletonSubst i a = S (IM.singleton i a)

-- | Turns a list into a substitution
listToSubst :: [(Int, Term)] -> Substitution
listToSubst = S . IM.fromListWith (error "Substitution: keys are not unique")

-- | Combines two substitutions. The left-hand side substitution is first applied to
-- the co-domain of the right-hand side substitution
(@@) :: Substitution -> Substitution -> Substitution
S a @@ S b = S $ a `IM.union` IM.map (S a |->) b

-- | Combines two substitutions with disjoint domains. If the domains are not disjoint,
-- an error is reported
(@@@) :: Substitution -> Substitution -> Substitution
S a @@@ S b = S (IM.unionWith err a b)
 where err _ _ = error "Unification.(@@@): domains of substitutions are not disjoint"

-- | Lookups a variable in a substitution. Nothing indicates that the variable is
-- not in the domain of the substitution
lookupVar :: Int -> Substitution -> Maybe Term
lookupVar s = IM.lookup s . unS

-- | Returns the domain of a substitution (as a list)
dom :: Substitution -> [Int]
dom = IM.keys . unS

-- | Apply the substitution
(|->) :: Substitution -> Term -> Term
s |-> term = 
   case term of
      Meta i -> fromMaybe term (lookupVar i s)
      _      -> f (map (s |->) cs)
 where
   (cs, f) = uniplate term