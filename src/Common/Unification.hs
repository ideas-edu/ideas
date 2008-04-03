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
   , HasVars(..), MakeVar(..), Substitutable(..), Unifiable(..)
   , noVars, match, unifyList, substitutePair
     -- * Quantification
   , ForAll, generalize, generalizeAll
   , instantiate, instantiateWith, unsafeInstantiate, unsafeInstantiateWith
   ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)
import Control.Monad

-----------------------------------------------------------
--- Substitution

infixr 4 |->
infixr 5 @@, @@@

-- | Abstract data type for substitutions
newtype Substitution a = S { unS :: M.Map String a }

instance Show a => Show (Substitution a) where
   show = show . unS

-- | Returns the empty substitution
emptySubst :: Substitution a
emptySubst = S M.empty

-- | Returns a singleton substitution
singletonSubst :: HasVars a => String -> a -> Substitution a
singletonSubst s a
   | s `S.member` getVars a = error "Unification.singletonSubst: occurs check failed"
   | otherwise = S (M.singleton s a)

-- | Turns a list into a substitution
listToSubst :: HasVars a => [(String, a)] -> Substitution a
listToSubst s
   | nub xs /= xs       = error "Unification.listToSubst: keys are not unique"
   | any (`elem` xs) ys = error "Unification.listToSubst: occurs check failed"
   | otherwise          = S (M.fromList s) 
 where
   xs = map fst s
   ys = concatMap (getVarsList . snd) s

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
lookupVar :: String -> Substitution a -> Maybe a
lookupVar s = M.lookup s . unS

-- | Returns the domain of a substitution (as a list)
dom :: Substitution a -> S.Set String
dom = M.keysSet . unS

-- | Returns the domain of a substitution (as a list)
domList :: Substitution a -> [String]
domList = M.keys . unS

-- | Removes variables from the domain of a substitution
removeDom :: S.Set String -> Substitution a -> Substitution a
removeDom s (S a) = S (M.filterWithKey (\k _ -> S.member k s) a)

-----------------------------------------------------------
--- Unification

-- | Type class for data types which have a variable. 
class HasVars a where
   getVars     :: a -> S.Set String
   getVarsList :: a -> [String]
   -- default definitions
   getVars     = S.fromList . getVarsList 
   getVarsList = S.toList   . getVars
   
-- | Type class for creating variables
class MakeVar a where
    makeVar    :: String -> a
    makeVarInt :: Int -> a
    -- default method
    makeVarInt = makeVar . ('_':) . show

-- | Type class for substitutable data types
class (HasVars a, MakeVar a) => Substitutable a where
   (|->) :: Substitution a -> a -> a    -- ^ The substitution operation

-- | Type class for unifiable data types
class Substitutable a => Unifiable a where
   unify :: a -> a -> Maybe (Substitution a)
  
instance HasVars a => HasVars [a] where
   getVars = S.unions . map getVars
   
instance (HasVars a, HasVars b) => HasVars (a, b) where
   getVars (x, y) = getVars x `S.union` getVars y

-- | Checks whether a value has no variables
noVars :: HasVars a => a -> Bool
noVars = S.null . getVars  

-- | Returns a substitution that unifies two lists. Nothing indicates that the
-- values cannot be unified, or that the lists are of different lengths.           
unifyList :: Unifiable a => [a] -> [a] -> Maybe (Substitution a)
unifyList xs ys = do 
      guard (length xs == length ys)
      foldr combine (return emptySubst) (zip xs ys)
    where
      combine (a, b) msub = do
        s1 <- msub
        s2 <- unify (s1 |-> a) (s1 |-> b)
        return (s1 @@@ s2)

-- | Applies a substitution on a pair
substitutePair :: (Substitutable a) => Substitution a -> (a, a) -> (a, a)
substitutePair sub (a, b) = (sub |-> a, sub |-> b)

-- | One-way unification: the right-hand operand should not be affected by
-- the returned substitution.
match :: Unifiable a => a -> a -> Maybe (Substitution a)
match a b = do
   s <- unify a b
   guard $ S.null $ dom s `S.intersection` getVars b
   return s
   
-----------------------------------------------------------
--- Quantification
   
data ForAll a = ForAll (S.Set String) a
   deriving Show

instance Functor ForAll where
   fmap f (ForAll s a) = ForAll s (f a)

-- | Quanity a term with a given set of variables
generalize :: [String] -> a -> ForAll a
generalize xs a = ForAll (S.fromList xs) a

-- | Quantify all free variables
generalizeAll :: HasVars a => a -> ForAll a
generalizeAll a = ForAll (getVars a) a

-- | Instantiate a quantified term using a value of type int to 
-- generate unique (fresh) variables
instantiate :: Substitutable a => Int -> ForAll a -> (a, Int)
instantiate = instantiateWith (|->)

-- | Same as instantiate, except that a special purpose function is passed to 
-- perform the substitution (instead of relying on the Substitutable type class)
instantiateWith :: (HasVars b, MakeVar b) => (Substitution b -> a -> a) -> Int -> ForAll a -> (a, Int)
instantiateWith f unique (ForAll s a) = (f sub a, unique + length vars)
 where 
   vars = S.toList s
   sub  = listToSubst $ zip vars (map makeVarInt [unique..])
      
-- | Instantiate a quantified term using a magic number (which is very large)
unsafeInstantiate :: Substitutable a => ForAll a -> a
unsafeInstantiate = unsafeInstantiateWith (|->)

-- | Same as unsafeInstantiate, except that a special purpose function is passed to
-- perform the substitution
unsafeInstantiateWith :: (HasVars b, MakeVar b) => (Substitution b -> a -> a) -> ForAll a -> a
unsafeInstantiateWith f = fst . instantiateWith f 12345