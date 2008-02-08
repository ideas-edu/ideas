module Common.Unification 
   ( Substitution, emptySubst, singletonSubst, listToSubst, (@@), (@@@), lookupVar, dom, domList, noVars
   , HasVars(..), MakeVar(..), Substitutable(..), Unifiable(..)
   , match, unifyList, substitutePair
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

domList :: Substitution a -> [String]
domList = M.keys . unS

removeDom :: S.Set String -> Substitution a -> Substitution a
removeDom s (S a) = S (M.filterWithKey (\k _ -> S.member k s) a)

class HasVars a where
   getVars     :: a -> S.Set String
   getVarsList :: a -> [String]
   -- default definitions
   getVars     = S.fromList . getVarsList 
   getVarsList = S.toList   . getVars

noVars :: HasVars a => a -> Bool
noVars = S.null . getVars
   
class MakeVar a where
    makeVar    :: String -> a
    makeVarInt :: Int -> a
    -- default method
    makeVarInt = makeVar . ('_':) . show
   
class (HasVars a, MakeVar a) => Substitutable a where
   (|->) :: Substitution a -> a -> a -- substitution

class Substitutable a => Unifiable a where
   unify :: a -> a -> Maybe (Substitution a)
   
instance HasVars a => HasVars [a] where
   getVars = S.unions . map getVars
   
instance (HasVars a, HasVars b) => HasVars (a, b) where
   getVars (x, y) = getVars x `S.union` getVars y
              
unifyList :: Unifiable a => [a] -> [a] -> Maybe (Substitution a)
unifyList xs ys = do 
      guard (length xs == length ys)
      foldr combine (return emptySubst) (zip xs ys)
    where
      combine (a, b) msub = do
        s1 <- msub
        s2 <- unify (s1 |-> a) (s1 |-> b)
        return (s1 @@@ s2)

substitutePair :: (Substitutable a) => Substitution a -> (a, a) -> (a, a)
substitutePair sub (a, b) = (sub |-> a, sub |-> b)

match :: Unifiable a => a -> a -> Maybe (Substitution a)
match a b = do
   s <- unify a b
   guard $ S.null $ dom s `S.intersection` getVars b
   return s
   
data ForAll a = ForAll (S.Set String) a
   deriving Show

instance Functor ForAll where
   fmap f (ForAll s a) = ForAll s (f a)
  
generalize :: [String] -> a -> ForAll a
generalize xs a = ForAll (S.fromList xs) a

generalizeAll :: HasVars a => a -> ForAll a
generalizeAll a = ForAll (getVars a) a

instantiate :: Substitutable a => Int -> ForAll a -> (a, Int)
instantiate = instantiateWith (|->)

instantiateWith :: MakeVar b => (Substitution b -> a -> a) -> Int -> ForAll a -> (a, Int)
instantiateWith f unique (ForAll s a) = (f sub a, unique + length vars)
 where 
   vars = S.toList s
   sub  = listToSubst $ zip vars (map makeVarInt [unique..])
      
-- Use a magic number for instantiation
unsafeInstantiate :: Substitutable a => ForAll a -> a
unsafeInstantiate = unsafeInstantiateWith (|->)

-- Use a magic number for instantiation
unsafeInstantiateWith :: MakeVar b => (Substitution b -> a -> a) -> ForAll a -> a
unsafeInstantiateWith f = fst . instantiateWith f 12345