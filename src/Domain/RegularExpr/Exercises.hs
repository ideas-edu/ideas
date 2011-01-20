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
module Domain.RegularExpr.Exercises (regexpExercise) where

import Common.Exercise
import Common.Navigator
import Common.Uniplate
import Common.Rewriting hiding (difference)
import Domain.RegularExpr.Expr
import Domain.RegularExpr.Parser
import Domain.RegularExpr.Strategy
import Domain.RegularExpr.Definitions
import Control.Monad
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen

regexpExercise :: Exercise RegExp
regexpExercise = makeExercise
   { exerciseId     = describe "Rewrite a regular expression" $
                         newId "regexp.normalform"
   , status         = Experimental
   , parser         = parseRegExp
   , prettyPrinter  = ppRegExp
--   , equivalence    = eqRE
   , similarity     = equalRegExpA  -- modulo associativity
   , isReady        = deterministic
   , isSuitable     = \a -> length [ () | Atom _ <- universe a ] > 1
   , difference     = differenceMode eqRE
   , strategy       = deterministicStrategy
   , navigation     = navigator
--   , extraRules     :: [Rule (Context a)]  -- Extra rules (possibly buggy) not appearing in strategy
   , testGenerator  = Just startFormGen -- arbitrary
   , randomExercise = simpleGenerator startFormGen -- myGen
   , examples       = unGen (replicateM 15 startFormGen) (mkStdGen 2805) 5  
   }

-- | Equality modulo associativity of operators
equalRegExpA:: RegExp -> RegExp -> Bool
equalRegExpA p q = rec p == rec q
 where
   rec expr = case expr of
                 (a :*: b) :*: c -> rec (a :*: (b :*: c))
                 (a :|: b) :|: c -> rec (a :|: (b :|: c))
                 _ -> descend rec expr

-- myGen :: Gen RegExp
-- myGen = restrictGenerator (isSuitable regexpExercise) arbitrary

startFormGen :: Gen RegExp
startFormGen = do
   i  <- oneof $ map return [1..10]
   xs <- replicateM i $ do
      j  <- oneof $ map return [1..5]
      ys <- replicateM j $ oneof $ map (return . Atom . return) "abcd"
      return $ foldr1 (:*:) ys
   return $ foldr1 (:|:) xs   

-- equivalence of regular expressions
eqRE :: Eq a => RE a -> RE a -> Bool
eqRE = (==)

{-
checkUntil :: Ord a => Int -> RE a -> RE a -> Bool
checkUntil n r s = empty r == empty s && (n==0 || next)
 where
   make = groupBy eqFst . sortBy cmpFst . firsts
   eqFst  (a, _) (b, _) = a==b 
   cmpFst (a, _) (b, _) = compare a b
   
   as = make r
   bs = make s
   next = and ((length as == length bs) : zipWith f as bs)
   
   -- f ((a, _):
   f _ _ = False -}