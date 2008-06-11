-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- This module defines substitutions and a type class for types that support 
-- unification of two terms. The variables that are used for substitution and
-- unification are supposed to be represented by Strings.
--
-----------------------------------------------------------------------------
module Common.Unification 
   ( -- * Substitution
     Substitution, emptySubst, singletonSubst, listToSubst
   , (@@), (@@@), lookupVar, dom, domList, removeDom
    -- * Unification
   , HasMetaVars(..), MetaVar(..), Substitutable(..), Unifiable(..)
   , noMetaVars, metaVars, match, unifyList, substitutePair
     -- * Quantification
   , ForAll, generalize, generalizeAll
   , instantiate, instantiateWith, unsafeInstantiate, unsafeInstantiateWith
   ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)
import Common.Utils
import Control.Monad

-----------------------------------------------------------
--- Substitution

infixr 4 |->
infixr 5 @@, @@@

-- | Abstract data type for substitutions
newtype Substitution a = S { unS :: M.Map Int a }

instance Show a => Show (Substitution a) where
   show = show . unS

-- | Returns the empty substitution
emptySubst :: Substitution a
emptySubst = S M.empty

-- | Returns a singleton substitution
singletonSubst :: (MetaVar a, HasMetaVars a) => Int -> a -> Substitution a
singletonSubst i a
   | isMetaVar a == Just i = emptySubst
   | i `S.member` getMetaVars a = error "Unification.singletonSubst: occurs check failed"
   | otherwise = S (M.singleton i a)

-- | Turns a list into a substitution
listToSubst :: HasMetaVars a => [(Int, a)] -> Substitution a
listToSubst s
   | nub xs /= xs       = error "Unification.listToSubst: keys are not unique"
   | any (`elem` xs) ys = error "Unification.listToSubst: occurs check failed"
   | otherwise          = S (M.fromList s) 
 where
   xs = map fst s
   ys = concatMap (getMetaVarsList . snd) s

-- | Combines two substitutions. The left-hand side substitution is first applied to
-- the co-domain of the right-hand side substitution
(@@) :: Substitutable a => Substitution a -> Substitution a -> Substitution a
S a @@ S b = S $ a `M.union` M.map (S a |->) b

-- | Combines two substitutions with disjoint domains. If the domains are not disjoint,
-- an error is reported
(@@@) :: Substitution a -> Substitution a -> Substitution a
S a @@@ S b = S (M.unionWith err a b)
 where err _ _ = error "Unification.(@@@): domains of substitutions are not disjoint"

-- | Lookups a variable in a substitution. Nothing indicates that the variable is
-- not in the domain of the substitution
lookupVar :: Int -> Substitution a -> Maybe a
lookupVar s = M.lookup s . unS

-- | Returns the domain of a substitution (as a list)
dom :: Substitution a -> S.Set Int
dom = M.keysSet . unS

-- | Returns the domain of a substitution (as a list)
domList :: Substitution a -> [Int]
domList = M.keys . unS

-- | Removes variables from the domain of a substitution
removeDom :: S.Set Int -> Substitution a -> Substitution a
removeDom s (S a) = S (M.filterWithKey (\k _ -> S.member k s) a)

-----------------------------------------------------------
--- Unification

-- | Type class for data types which have meta-variables
class HasMetaVars a where
   getMetaVars     :: a -> S.Set Int
   getMetaVarsList :: a -> [Int]
   -- default definitions
   getMetaVars     = S.fromList . getMetaVarsList 
   getMetaVarsList = S.toList   . getMetaVars
   
-- | Type class for creating meta-variables
class MetaVar a where
    isMetaVar :: a -> Maybe Int
    metaVar   :: Int -> a

-- | Type class for substitutable data types
class (HasMetaVars a, MetaVar a) => Substitutable a where
   (|->) :: Substitution a -> a -> a    -- ^ The substitution operation

-- | Type class for unifiable data types
class Substitutable a => Unifiable a where
   unify    :: a -> a -> Maybe (Substitution a)
   -- unifyAll :: a -> a -> [Substitution a]
   -- default methods
   -- unify    x y = safeHead (unifyAll x y)
   -- unifyAll x y = maybe [] return (unify x y) 
   
instance HasMetaVars a => HasMetaVars [a] where
   getMetaVars = S.unions . map getMetaVars
   
instance (HasMetaVars a, HasMetaVars b) => HasMetaVars (a, b) where
   getMetaVars (x, y) = getMetaVars x `S.union` getMetaVars y

-- | Checks whether a value has no variables
noMetaVars :: HasMetaVars a => a -> Bool
noMetaVars = S.null . getMetaVars  

-- | Produces an infinite list of meta-variables
metaVars :: MetaVar a => [a]
metaVars = map metaVar [0..]

-- | Returns a substitution that unifies two lists. Nothing indicates that the
-- values cannot be unified, or that the lists are of different lengths.           
unifyList :: Unifiable a => [a] -> [a] -> Maybe (Substitution a)
unifyList xs ys = safeHead (unifyListAll xs ys)

-- | Returns all substitutions that unify two lists. The empty list indicates that the
-- values cannot be unified, or that the lists are of different lengths.           
unifyListAll :: Unifiable a => [a] -> [a] -> [Substitution a]
unifyListAll xs ys = do 
      guard (length xs == length ys)
      foldr combine (return emptySubst) (zip xs ys)
    where
      unifyAll x y = maybe [] return (unify x y) 
      combine (a, b) msub = do
        s1 <- msub
        s2 <- unifyAll (s1 |-> a) (s1 |-> b)
        return (s1 @@@ s2)
        
-- | Applies a substitution on a pair
substitutePair :: (Substitutable a) => Substitution a -> (a, a) -> (a, a)
substitutePair sub (a, b) = (sub |-> a, sub |-> b)

-- | One-way unification: the right-hand operand should not be affected by
-- the returned substitution.
match :: Unifiable a => a -> a -> Maybe (Substitution a)
match a b = safeHead (matchAll a b)

-- | One-way unification: the right-hand operand should not be affected by
-- the returned substitution.
matchAll :: Unifiable a => a -> a -> [Substitution a]
matchAll a b = do
   s <- unifyAll a b
   guard $ S.null $ dom s `S.intersection` getMetaVars b
   return s
 where
   unifyAll x y = maybe [] return (unify x y) 
      
-----------------------------------------------------------
--- Quantification
   
data ForAll a = ForAll (S.Set Int) a
   deriving Show

instance Functor ForAll where
   fmap f (ForAll s a) = ForAll s (f a)

-- | Quanity a term with a given set of variables
generalize :: [Int] -> a -> ForAll a
generalize xs a = ForAll (S.fromList xs) a

-- | Quantify all free variables
generalizeAll :: HasMetaVars a => a -> ForAll a
generalizeAll a = ForAll (getMetaVars a) a

-- | Instantiate a quantified term using a value of type int to 
-- generate unique (fresh) variables
instantiate :: Substitutable a => Int -> ForAll a -> (a, Int)
instantiate = instantiateWith (|->)

-- | Same as instantiate, except that a special purpose function is passed to 
-- perform the substitution (instead of relying on the Substitutable type class)
instantiateWith :: (HasMetaVars b, MetaVar b) => (Substitution b -> a -> a) -> Int -> ForAll a -> (a, Int)
instantiateWith f unique (ForAll s a) = (f sub a, unique + length vars)
 where 
   vars = S.toList s
   sub  = S $ M.fromList $ zip vars (map metaVar [unique..])
      
-- | Instantiate a quantified term using a magic number (which is very large)
unsafeInstantiate :: Substitutable a => ForAll a -> a
unsafeInstantiate = unsafeInstantiateWith (|->)

-- | Same as unsafeInstantiate, except that a special purpose function is passed to
-- perform the substitution
unsafeInstantiateWith :: (HasMetaVars b, MetaVar b) => (Substitution b -> a -> a) -> ForAll a -> a
unsafeInstantiateWith f = fst . instantiateWith f 12345