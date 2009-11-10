-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Common.Strategy.BiasedChoice 
   ( Bias(..), placeBiasLabels, biasTreeG, makeBiasedTree
   ) where

import Common.Apply
import Common.Derivation
import Common.Transformation
import Common.Strategy.Core
import Common.Uniplate

data Bias f a = TryFirst BiasId | OrElse BiasId | Normal (f a) deriving Show
type BiasId = Int

instance Apply f => Apply (Bias f) where
   applyAll (Normal r) = applyAll r
   applyAll _          = return

placeBiasLabels :: Core l a -> Core (Either (Bias f a) l) a
placeBiasLabels = rec 0 . mapLabel Right
 where
   rec n (a :|>: b) = 
      let left  = Label (Left (TryFirst n)) (rec n a)
          right = Label (Left (OrElse n))   (rec (n+count a) b)
      in left :|: right
   rec n core = 
      let  (cs, f) = uniplate core 
      in f (recList n cs)
      
   recList _ [] = []
   recList n (x:xs) = rec n x : recList (n+count x) xs
   
   count a = length [ () | _ :|>: _ <- universe a ]

biasTranslation :: (Rule a -> f a) -> Translation (Either (Bias f a) l) a (Bias f a)
biasTranslation f = (either Before (const Skip), Normal . f)

biasTreeG :: (DerivationTree (f a, info) a -> Bool) -> DerivationTree (Bias f a, info) a -> DerivationTree (f a, info) a
biasTreeG success t = t {branches = f [] (branches t)}
 where
   f _ [] = []
   f env (((bias, info), st):xs) = 
      case bias of
         TryFirst n
            | success new -> branches new ++ f (n:env) xs
            | otherwise   -> f env xs
          where new = biasTreeG success st
         OrElse n   
            | n `elem` env -> f env xs
            | otherwise    -> branches (biasTreeG success st) ++ f env xs
         Normal r   -> ((r, info), biasTreeG success st):f env xs

--   success :: DerivationTree s a -> Bool
--   success = isJust . derivation

biasTree :: (DerivationTree (f a) a -> Bool) -> DerivationTree (Bias f a) a -> DerivationTree (f a) a
biasTree success t = t {branches = f [] (branches t)}
 where
   f _ [] = []
   f env ((bias, st):xs) = 
      case bias of
         TryFirst n
            | success new -> branches new ++ f (n:env) xs
            | otherwise   -> f env xs
          where new = biasTree success st
         OrElse n   
            | n `elem` env -> f env xs
            | otherwise    -> branches (biasTree success st) ++ f env xs
         Normal r   -> (r, biasTree success st):f env xs
{-
   success :: DerivationTree s a -> Bool
   success = isJust . derivation -}
   
makeBiasedTree :: (DerivationTree (Rule a) a -> Bool) -> Core l a -> a -> DerivationTree (Rule a) a
makeBiasedTree p core = 
   biasTree p . changeLabel fst . runTree (strategyTree (biasTranslation id) (placeBiasLabels core))
    
-------------------------
test = makeBiasedTree (maybe False (const True) . derivation) myCore 5

myCore = (Rule r1 :|>: Rule r2) :|: (Rule r3 :|>: Rule r4)

r1, r2, r3, r4 :: Rule Int
r1 = make "r1" $ \n -> trace "**1**" [n*n]
r2 = make "r2" $ \n -> trace "**2**" [n+1]
r3 = make "r3" $ \n -> trace "**3**" [n*2]
r4 = make "r4" $ \n -> trace "**4**" [n `div` 2]
trace _ = id
make n = minorRule . makeSimpleRuleList n