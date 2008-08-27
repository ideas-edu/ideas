module Domain.Math.Rewriting where

import Common.Utils
import Common.Context
import Domain.Math.Classes
import Domain.Math.Rules
import Domain.Math.Constrained
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.IntMap as IM

-----------------------------------------------------------
-- AC theories

type OperatorAC a = (a -> Maybe (a, a), a -> a -> a)

collectAC :: OperatorAC a -> a -> [a]
collectAC (f, _) a = rec a []
 where
   rec a = case f a of
              Just (x, y) -> rec x . rec y
              Nothing     -> (a:)

buildAC :: OperatorAC a -> [a] -> a
buildAC (_, f) = foldr1 f

testAC :: OperatorAC a -> a -> Bool
testAC (f, _) = isJust . f

findOperatorAC :: [OperatorAC a] -> a -> Maybe (OperatorAC a)
findOperatorAC acs a = safeHead $ filter (`testAC` a) acs 

normalizeACs :: (Uniplate a, Ord a) => [OperatorAC a] -> a -> a
normalizeACs acs = rec
 where
   rec a = 
      case findOperatorAC acs a of
         Just ac -> 
            buildAC ac $ sort $ map rec $ collectAC ac a
         Nothing -> 
            let (cs, f) = uniplate a
            in f (map rec cs)

normalizeAC :: (Uniplate a, Ord a) => OperatorAC a -> a -> a
normalizeAC ac = normalizeACs [ac]

equalACs :: (Uniplate a, Ord a) => [OperatorAC a] -> a -> a -> Bool
equalACs acs x y = normalizeACs acs x == normalizeACs acs y

equalAC :: (Uniplate a, Ord a) => OperatorAC a -> a -> a -> Bool
equalAC ac = equalACs [ac]

-----------------------------------------------------------
-- Substitution

type Subst a = IM.IntMap a

renumberVars :: (Uniplate a, MetaVar a) => a -> a
renumberVars a = s |-> a
 where s = IM.fromList $ zip (freeVars a) (map metaVar [0..])
 
(|->) :: (MetaVar a, Uniplate a) => Subst a -> a -> a
s |-> e = 
   case isMetaVar e of
      Just i  -> maybe e id $ IM.lookup i s
      Nothing -> let (cs, f) = uniplate e
                 in f (map (s |->) cs)
                 
-----------------------------------------------------------
-- Unification

unify :: (MetaVar a, UniplateConstr a) => a -> a -> Maybe (Subst a)
unify x y = safeHead (unifyAC [] x y)

unifyM :: (Monad m, MetaVar a, UniplateConstr a) => a -> a -> m (Subst a)
unifyM x y = maybe (fail "unify") return (unify x y)  

unifyAC :: (MetaVar a, UniplateConstr a) => [OperatorAC a] -> a -> a -> [Subst a]
unifyAC acs x y = 
   case (isMetaVar x, isMetaVar y) of
      (Just i, Just j) | i==j -> return IM.empty
      (Just i, _) | i `notElem` freeVars y -> return $ IM.singleton i y
      (_, Just j) | j `notElem` freeVars x -> return $ IM.singleton j x 
      _ | shallowEq x y ->
             case findOperatorAC acs x of
                Just ac -> 
                   let pairs = combine (collectAC ac x) (collectAC ac y)
                       make xs = 
                          let f = buildAC ac
                              (as, bs) = unzip xs
                          in unifyListAC acs (map f as) (map f bs)
                   in concatMap make pairs
                Nothing -> 
                  unifyListAC acs (children x) (children y)             
        | otherwise ->
             []

unifyListAC :: (MetaVar a, UniplateConstr a) => [OperatorAC a] -> [a] -> [a] -> [Subst a]
unifyListAC _ []     []     = return IM.empty
unifyListAC acs (x:xs) (y:ys) = do
   s1 <- unifyAC acs x y
   let f = map (s1 |->)
   s2 <- unifyListAC acs (f xs) (f ys)
   return (s1 `IM.union` s2)
unifyListAC _ _ _ = []

-- Helper-functions
combine :: [a] -> [b] -> [[([a], [b])]]
combine as bs 
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
-- Matching

match :: (MetaVar a, UniplateConstr a) => Rule a -> a -> Maybe (a, Prop (Con a))
match r e = safeHead (matchAC [] r e)

matchM :: (Monad m, MetaVar a, UniplateConstr a) => Rule a -> a -> m (a, Prop (Con a))
matchM r e = maybe (fail "match") return (match r e) 
      
matchAC :: (MetaVar a, UniplateConstr a) => [OperatorAC a] -> Rule a -> a -> [(a, Prop (Con a))]
matchAC acs r e = do 
   let Triple lhs rhs p0 = ruleTriple r (nextVar e)
       wfs = [ return (WF (metaVar a)) | a <- freeVars lhs \\ freeVars rhs ]
       p   = mconcat (p0:wfs)
   s <- unifyAC acs lhs e
   if any (`IM.member` s) (freeVars e)
      then []
      else return (s |-> rhs, fmap (fmap (s |->)) p)

normalFormAC :: (MetaVar a, UniplateConstr a, Ord a) => [OperatorAC a] -> [Rule a] -> a -> a
normalFormAC acs rs = fixpoint $ transformBU $ \a ->
   case [ b | r <- rs, (b, _) <- matchAC acs r a ] of
      hd:_ -> normalizeACs acs hd
      _    -> a
      
normalForm :: (MetaVar a, UniplateConstr a, Ord a) => [Rule a] -> a -> a
normalForm = normalFormAC []
      
-----------------------------------------------------------
-- Traversal functions: contains "general" uniplate functions -> clean up

universeLocation :: Uniplate a => a -> [([Int], a)]
universeLocation a = ([], a) : [ (i:is, b) | (i, c) <- zip [0..] (children a), (is, b) <- universeLocation c ]

transformBU :: Uniplate a => (a -> a) -> a -> a
transformBU g a = g $ f $ map (transformBU g) cs
 where
   (cs, f) = uniplate a

transformM :: (Monad m, Uniplate a) => (a -> m a) -> a -> m a
transformM g a = mapM (transformM g) cs >>= (g . f)
 where
   (cs, f) = uniplate a

oneM :: (MonadPlus m, Uniplate a) => (a -> m a) -> a -> m a
oneM f a = msum (f a : zipWith make [0..] (children a))
 where
   make i = applyAtM [i] (oneM f)
   
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = stop . iterate f 
 where
   stop (x:xs) 
      | x == head xs = x
      | otherwise    = stop xs
      
fixpointM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixpointM f a = do
   b <- f a
   if a==b then return a else fixpointM f b

-- helper function
applyAt :: Uniplate a => [Int] -> (a -> a) -> a -> a
applyAt is f a = fromMaybe a (applyAtM is (return . f) a)

applyAtM :: (Monad m, Uniplate a) => [Int] -> (a -> m a) -> a -> m a
applyAtM [] f a     = f a
applyAtM (i:is) f a =
   case splitAt i cs of
      (xs, y:ys) -> do
         z <- applyAtM is f y
         return $ g $ xs ++ [z] ++ ys
      _ -> fail "applyAt"
 where
   (cs, g) = uniplate a

composQ :: Uniplate b => a -> (a -> a -> a) -> (b -> a) -> b -> a
composQ zero combine f = foldr (combine . f) zero . children