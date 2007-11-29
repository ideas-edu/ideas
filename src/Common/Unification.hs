{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -fglasgow-exts #-}
module Common.Unification 
   ( Substitution, emptySubst, singletonSubst, listToSubst, (@@), (@@@), lookupVar, dom
   , HasVars(..), MakeVar(..), BiSubstitutable(..), Substitutable(..), {-BiUnifiable(..),-} Unifiable(..)
   , (|->), match, unifyList
   , ForAll, generalize, generalizeAll, instantiate, instantiateWith, unsafeInstantiate, unsafeInstantiateWith
   ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

infixr 4 |->
infixr 5 @@, @@@

newtype Substitution a = S { unS :: M.Map String a }
   deriving Show

emptySubst :: Substitution a
emptySubst = S M.empty

singletonSubst :: String -> a -> Substitution a
singletonSubst s a = S (M.singleton s a)

listToSubst :: [(String, a)] -> Substitution a
listToSubst = S . M.fromList

(@@) :: Substitutable a => Substitution a -> Substitution a -> Substitution a
S a @@ S b = S $ a `M.union` M.map (S a |->) b

(@@@) :: Substitution a -> Substitution a -> Substitution a
S a @@@ S b = S (M.unionWith err a b)
 where err _ _ = error "Unification.(@@@): domains of substitutions are not disjoint"

lookupVar :: String -> Substitution a -> Maybe a
lookupVar s = M.lookup s . unS

dom :: Substitution a -> S.Set String
dom = M.keysSet . unS

removeDom :: S.Set String -> Substitution a -> Substitution a
removeDom s (S a) = S (M.filterWithKey (\k _ -> S.member k s) a)

class HasVars a where
   getVars :: a -> S.Set String

class MakeVar a where
    makeVar    :: String -> a
    makeVarInt :: Int -> a
    -- default method
    makeVarInt = makeVar . ('_':) . show
   
class (MakeVar a, HasVars sa) => BiSubstitutable a sa where
   biSubstitute :: Substitution a -> sa -> sa

class BiSubstitutable a a => Substitutable a where
   substitute :: Substitution a -> a -> a

--class BiUnifiable a sa where
--   biUnify :: sa -> sa -> Maybe (Substitution a)
   
class (Substitutable a{-, BiUnifiable a a-}) => Unifiable a where
   unify :: a -> a -> Maybe (Substitution a)
   
instance HasVars a => HasVars [a] where
   getVars = S.unions . map getVars
   
instance (HasVars a, HasVars b) => HasVars (a, b) where
   getVars (x, y) = getVars x `S.union` getVars y

-- default instance
--instance (HasVars a, Substitutable a) => BiSubstitutable a a where
--   biSubstitute = substitute

instance (BiSubstitutable a b, BiSubstitutable a c) => BiSubstitutable a (b, c) where
   biSubstitute s (x, y) = (biSubstitute s x, biSubstitute s y) 
   
instance BiSubstitutable a b => BiSubstitutable a [b] where
   biSubstitute = map . biSubstitute
 
-- default instance
{- instance Unifiable a => BiUnifiable a a where
   biUnify = unify
  
instance (BiUnifiable a b, BiSubstitutable a b, BiUnifiable a c, BiSubstitutable a c) => BiUnifiable a (b, c) where
   biUnify (b1, c1) (b2, c2) = do 
      s1 <- biUnify b1 b2
      s2 <- biUnify (s1 |-> c1) (s1 |-> c2)
      return (s1 @@@ s2)
      
instance (BiUnifiable a b, BiSubstitutable a b) => BiUnifiable a [b] where
   biUnify xs ys = do 
      guard (length xs == length ys)
      foldr combine (return emptySubst) (zip xs ys)
    where
      combine (a, b) msub = do
        s1 <- msub
        s2 <- biUnify (s1 |-> a) b
        return (s1 @@@ s2) -}
              
(|->) :: BiSubstitutable a sa => Substitution a -> sa -> sa
(|->) = biSubstitute

unifyList :: Unifiable a => [a] -> [a] -> Maybe (Substitution a)
unifyList xs ys = do 
      guard (length xs == length ys)
      foldr combine (return emptySubst) (zip xs ys)
    where
      combine (a, b) msub = do
        s1 <- msub
        s2 <- unify (s1 |-> a) b
        return (s1 @@@ s2)

match :: Unifiable a => a -> a -> Maybe (Substitution a)
match a b = do
   s <- unify a b
   guard $ S.null $ dom s `S.intersection` getVars b
   return s
   
data ForAll a = ForAll (S.Set String) a
   deriving Show
   
generalize :: [String] -> a -> ForAll a
generalize xs a = ForAll (S.fromList xs) a

generalizeAll :: HasVars a => a -> ForAll a
generalizeAll a = ForAll (getVars a) a

instantiate :: Substitutable a => Int -> ForAll a -> (a, Int)
instantiate unique (ForAll s a) = (substitute sub a, unique + length vars)
 where 
   vars = S.toList s
   sub  = listToSubst $ zip vars (map makeVarInt [unique..])

instantiateWith :: BiSubstitutable b a => (Int -> b) -> Int -> ForAll a -> (a, Int)
instantiateWith f unique (ForAll s a) = (biSubstitute sub a, unique + length vars)
 where 
   vars = S.toList s
   sub  = listToSubst $ zip vars (map f [unique..])
      
-- Use a magic number for instantiation
unsafeInstantiate :: Substitutable a => ForAll a -> a
unsafeInstantiate = fst . instantiate 12345

-- Use a magic number for instantiation
unsafeInstantiateWith :: BiSubstitutable b a => (Int -> b) -> ForAll a -> a
unsafeInstantiateWith f = fst . instantiateWith f 12345

instance HasVars a => HasVars (ForAll a) where
   getVars (ForAll s a) = getVars a S.\\ s

instance BiSubstitutable a b => BiSubstitutable a (ForAll b) where
   biSubstitute s (ForAll xs a) = ForAll xs (biSubstitute (removeDom xs s) a)