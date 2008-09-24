module Common.Rewriting.Unification 
   ( ShallowEq(..), Rewrite(..)
   , unify, unifyM, unifyWith
   , match, matchM, matchWith
   ) where

import Common.Uniplate
import Common.Rewriting.MetaVar
import Common.Rewriting.Substitution
import Common.Rewriting.AC
import Control.Monad
import qualified Data.IntSet as IS
import Test.QuickCheck

-----------------------------------------------------------
-- Unification (in both ways)

class ShallowEq a where 
   shallowEq :: a -> a -> Bool

-- The arbitrary type class is a quick solution to have smart generators
-- (in combination with lifting rules). The function in the RewriteRule module
-- cannot have a type class for this reason
-- The show type class is added for pretty-printing rules
class (MetaVar a, Uniplate a, ShallowEq a, Arbitrary a, Show a) => Rewrite a where
   operators :: [Operator a]
   -- default definition: no associative/commutative operators
   operators = []

unify :: Rewrite a => a -> a -> [Substitution a]
unify = unifyWith operators

unifyM :: (MonadPlus m, Rewrite a) => a -> a -> m (Substitution a)
unifyM x y = msum $ map return $ unify x y

unifyWith :: Rewrite a => [Operator a] -> a -> a -> [Substitution a]
unifyWith ops = rec
 where
   rec x y =
      case (isMetaVar x, isMetaVar y) of
         (Just i, Just j) | i==j -> return emptySubst
         (Just i, _) | not (hasMetaVar i y) -> return $ singletonSubst i y
         (_, Just j) | not (hasMetaVar j x) -> return $ singletonSubst j x
         _ -> do
            guard (shallowEq x y) 
            case findOperator ops x of
               Just op -> 
                  concatMap (uncurry recList . unzip) (pairings op x y)
               Nothing -> 
                  recList (children x) (children y)    

   recList [] []    = return emptySubst
   recList (x:xs) (y:ys) = do
      s1 <- rec x y
      s2 <- recList (map (s1 |->) xs) (map (s1 |->) ys)
      return (s2 @@ s1)
   recList _ _ = []

-----------------------------------------------------------
-- Matching (or: one-way unification)

match :: Rewrite a => a -> a -> [Substitution a]
match = matchWith operators

matchM :: (MonadPlus m, Rewrite a) => a -> a -> m (Substitution a)
matchM x y = msum $ map return $ match x y

matchWith :: Rewrite a => [Operator a] -> a -> a -> [Substitution a]
matchWith ops x y = do
   s <- rec x y
   guard (IS.null $ dom s `IS.intersection` getMetaVars y)
   return s
 where
   rec x y =
      case isMetaVar x of
         Just i | not (hasMetaVar i y) -> return $ singletonSubst i y
         _ -> do
            guard (shallowEq x y) 
            case findOperator ops x of
               Just op -> 
                  concatMap (uncurry recList . unzip) (pairingsMatch op x y)
               Nothing -> 
                  recList (children x) (children y)    

   recList [] [] = return emptySubst
   recList (x:xs) (y:ys) = do
      s1 <- rec x y
      s2 <- recList (map (s1 |->) xs) (map (s1 |->) ys)
      return (s2 @@ s1)
   recList _ _ = []