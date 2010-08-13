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
-----------------------------------------------------------------------------
module Common.Rewriting.Unification (match) where

import Common.Rewriting.Term
import Common.Rewriting.AC
import Common.Rewriting.Substitution
import Control.Monad
-- import qualified Data.Map as M

-----------------------------------------------------------
-- Unification (in both ways)
{-
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
-}
-----------------------------------------------------------
-- Matching (or: one-way unification)

match :: [Symbol] -> Term -> Term -> [Substitution]
match assocSymbols x y = do
   s <- rec x y
   guard $ all (`notElem` getMetaVars y) (dom s)
   return s
 where
   rec (Meta i) y = do 
      guard (not (hasMetaVar i y))
      return (singletonSubst i y)

   rec x y = do
      let (a, as) = getSpine x
          (b, bs) = getSpine y
      case isCon a of
         Just s | s `elem` assocSymbols ->
            concatMap (uncurry recList . unzip) (associativeMatch s x y)
         _ -> do
            guard (a == b)
            recList as bs

   recList [] [] = return emptySubst
   recList (x:xs) (y:ys) = do
      s1 <- rec x y
      s2 <- recList (map (s1 |->) xs) (map (s1 |->) ys)
      return (s2 @@ s1)
   recList _ _ = []
      
associativeMatch :: Symbol -> Term -> Term -> [[(Term, Term)]]
associativeMatch s a b = map (map make) result
 where
   (as, bs) = onBoth collect (a, b)
   result   = pairingsA2 True as bs
   make     = onBoth construct
   
   collect = ($ []) . rec
    where 
      rec term =
         case isBinary s term of
            Just (a, b) -> rec a . rec b
            Nothing     -> (term:)
   
   construct xs 
      | null xs   = error "associativeMatcher: empty list"
      | otherwise = foldr1 (binary s) xs