module Common.Rewriting.Unification 
   ( ShallowEq(..), Rewrite(..)
   , unify, unifyM, unifyAC
   , match, matchM, matchAC
   ) where

import Common.Utils
import Common.Uniplate
import Common.Rewriting.MetaVar
import Common.Rewriting.Substitution
import Common.Rewriting.AC
import qualified Data.IntSet as IS

-----------------------------------------------------------
-- Unification

class ShallowEq a where 
   shallowEq :: a -> a -> Bool

class (MetaVar a, Uniplate a, ShallowEq a) => Rewrite a where
   operatorsAC :: [OperatorAC a]
   -- default definition: no associative/commutative operators
   operatorsAC = []

unify :: Rewrite a => a -> a -> Maybe (Substitution a)
unify x y = safeHead (unifyAC [] x y)

unifyM :: (Monad m, Rewrite a) => a -> a -> m (Substitution a)
unifyM x y = maybe (fail "unify") return (unify x y)  

unifyAC :: Rewrite a => [OperatorAC a] -> a -> a -> [Substitution a]
unifyAC acs x y = 
   case (isMetaVar x, isMetaVar y) of
      (Just i, Just j) | i==j -> return emptySubst
      (Just i, _) | not (hasMetaVar i y) -> return $ singletonSubst i y
      (_, Just j) | not (hasMetaVar j x) -> return $ singletonSubst j x 
      _ | shallowEq x y ->
             case findOperatorAC acs x of
                Just ac -> 
                   let pairs = combineA (collectAC ac x) (collectAC ac y)
                       make xs = 
                          let f = buildAC ac
                              (as, bs) = unzip xs
                          in unifyListAC acs (map f as) (map f bs)
                   in concatMap make pairs
                Nothing -> 
                  unifyListAC acs (children x) (children y)             
        | otherwise ->
             []

unifyListAC :: Rewrite a => [OperatorAC a] -> [a] -> [a] -> [Substitution a]
unifyListAC _ []     []     = return emptySubst
unifyListAC acs (x:xs) (y:ys) = do
   s1 <- unifyAC acs x y
   let f = map (s1 |->)
   s2 <- unifyListAC acs (f xs) (f ys)
   return (s1 @@@ s2)
unifyListAC _ _ _ = []

-- Helper-functions
combineA :: [a] -> [b] -> [[([a], [b])]]
combineA [] _ = []
combineA _ [] = []
combineA [a] bs = [[([a], bs)]]
combineA as [b] = [[(as, [b])]]
combineA (a:as) (b:bs) = 
      concat [ map (([a], bs1):) (combineA as bs2) | i <- [1 .. length bs], let (bs1, bs2) = splitAt i (b:bs) ]
   ++ concat [ map ((as1, [b]):) (combineA as2 bs) | i <- [2 .. length as], let (as1, as2) = splitAt i (a:as) ]
   
combineAC :: [a] -> [b] -> [[([a], [b])]]
combineAC as bs 
   | length as < length bs = map (map (\(x,y) -> (y,x))) $ rec bs as
   | otherwise = rec as bs
 where
   rec as [] = if null as then [[]] else []
   rec as (b:bs) = concat [ map ((as1, [b]):) (rec as2 bs) | (as1, as2) <- splits as, not (null as1) ]

splits :: [a] -> [([a], [a])]
splits = foldr insert [([], [])]
 where
   insert a xs = map (toLeft a) xs ++ map (toRight a) xs
   toLeft  a (xs, ys) = (a:xs, ys)
   toRight a (xs, ys) = (xs, a:ys) 
   
-----------------------------------------------------------
-- Matching (or: one-way unification)

match :: Rewrite a => a -> a -> Maybe (Substitution a)
match x y = safeHead (matchAC [] x y)

matchM :: (Monad m, Rewrite a) => a -> a -> m (Substitution a)
matchM x y = maybe (fail "unify") return (match x y)  

matchAC :: Rewrite a => [OperatorAC a] -> a -> a -> [Substitution a]
matchAC acs x y = filter oneway (unifyAC acs x y)
 where
   oneway s = IS.null (getMetaVars y `IS.intersection` dom s)