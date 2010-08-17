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
module Common.Rewriting.Unification (match, specialLeft, specialRight) where

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

-- second term should not have meta variables

match :: [Symbol] -> Term -> Term -> [Substitution]
match assocSymbols x y = do
   s <- rec True x y
  -- guard $ all (`notElem` getMetaVars y) (dom s)
   return s
 where
   rec _ (Meta i) y = do 
  -- guard (not (hasMetaVar i y))
      return (singletonSubst i y)

   rec isTop x y = do
      case getSpine x of
         (Con s, [a1, a2]) | s `elem` assocSymbols ->
            concatMap (uncurry recList . unzip) (associativeMatch isTop s a1 a2 y)
         (a, as) -> do
            let (b, bs) = getSpine y
            guard (a == b)
            recList as bs

   recList [] [] = return emptySubst
   recList (x:xs) (y:ys) = do
      s1 <- rec False x y
      s2 <- recList (map (s1 |->) xs) (map (s1 |->) ys)
      return (s2 @@@ s1)
   recList _ _ = []
      
associativeMatch :: Bool -> Symbol -> Term -> Term -> Term -> [[(Term, Term)]]
associativeMatch isTop s a1 a2 (Apply (Apply (Con t) b1) b2) 
   | s==t = map (map make) result
 where
   as = collect a1 . collect a2 $ []
   bs = collect b1 . collect b2 $ []
   list | isTop     = map ($ as) [id, extLeft, extRight, extBoth]
        | otherwise = [as]
        
   extLeft  = (Meta specialLeft:)
   extRight = (++[Meta specialRight])
   extBoth  = extLeft . extRight
   
   result   = concatMap (\zs -> pairingsA2 True zs bs) list
   make     = onBoth construct
   
   collect term =
      case isBinary s term of
         Just (a, b) -> collect a . collect b
         Nothing     -> (term:)
   
   construct xs 
      | null xs   = error "associativeMatcher: empty list"
      | otherwise = foldr1 (binary s) xs
associativeMatch _ _ _ _ _ = []

specialLeft, specialRight :: Int -- special meta variables for context extension
specialLeft  = maxBound
specialRight = pred specialLeft