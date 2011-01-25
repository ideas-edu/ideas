{-# LANGUAGE TypeSynonymInstances #-}
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
module Domain.Logic.Generator
   ( generateLogic, generateLevel, equalLogicA, Level(..)
   ) where

import Common.Algebra.Boolean
import Common.Utils (ShowString(..))
import Domain.Logic.Formula
import Control.Monad
import Data.Char
import Test.QuickCheck
import Common.Rewriting
import Common.Uniplate

-------------------------------------------------------------
-- Code that doesn't belong here, but the arbitrary instance
-- is needed for the Rewrite instance.

instance Rewrite SLogic where
   associativeOps = [andOperator, orOperator]

-- | Equality modulo associativity of operators
equalLogicA :: Eq a => Logic a -> Logic a -> Bool
equalLogicA p q = rec p == rec q
 where
   rec a = case a of
              _ :&&: _ -> ands (map rec (conjunctions a))
              _ :||: _ -> ors  (map rec (disjunctions a))
              _        -> descend rec a

-----------------------------------------------------------
-- Logic generator

data Level = Easy | Normal | Difficult 
   deriving Show

generateLogic :: Gen SLogic
generateLogic = normalGenerator

generateLevel :: Level -> (Gen SLogic, (Int, Int))
generateLevel level =
   case level of
      Easy      -> (easyGenerator,      (3, 6))
      Normal    -> (normalGenerator,    (4, 12))
      Difficult -> (difficultGenerator, (7, 18))

-- Use the propositions with 3-6 steps
easyGenerator :: Gen SLogic 
easyGenerator = do
   n  <- oneof [return 2, return 4] -- , return 8]
   sizedGen True varGen n

-- Use the propositions with 4-12 steps
normalGenerator :: Gen SLogic
normalGenerator = do
   p0 <- sizedGen False varGen 4
   p1 <- preventSameVar varList p0
   return (removePartsInDNF p1)

-- Use the propositions with 7-18 steps
difficultGenerator :: Gen SLogic
difficultGenerator = do
   let vs = ShowString "s" : varList
   p0 <- sizedGen False (oneof $ map return vs) 4
   p1 <- preventSameVar vs p0
   return (removePartsInDNF p1)

varList :: [ShowString]
varList = map ShowString ["p", "q", "r"]

varGen :: Gen ShowString
varGen = oneof $ map return varList

sizedGen :: Bool -> Gen a -> Int -> Gen (Logic a)
sizedGen constants gen = go 
 where
   go n
      | n > 0 =
           let rec   = go (n `div` 2)
               op2 f = liftM2 f rec rec
           in frequency 
                 [ (2, go 0)
                 , (2, op2 (:->:))
                 , (1, op2 (:<->:))
                 , (3, op2 (:&&:))
                 , (3, op2 (:||:))
                 , (3, liftM Not rec)
                 ]
      | constants = frequency
           [(5, liftM Var gen), (1, return T), (1, return F)]
      | otherwise = liftM Var gen

-----------------------------------------------------------------
-- Simple tricks for creating for "nice" logic propositions

preventSameVar :: Eq a => [a] -> Logic a -> Gen (Logic a)
preventSameVar xs = rec 
 where
   rec p = case holes p of
              [(Var a, _), (Var b, update)] | a==b -> do
                 c <- oneof $ map return $ filter (/=a) xs
                 return $ update (Var c)
              _ -> descendM rec p

removePartsInDNF :: SLogic -> SLogic
removePartsInDNF = buildOr . filter (not . simple) . disjunctions
 where
   buildOr [] = T
   buildOr xs = foldl1 (:||:) xs
   
   simple = all f . conjunctions
    where
      f (Not p) = null (children p)
      f p       = null (children p) 

-----------------------------------------------------------
--- QuickCheck generator

instance Arbitrary SLogic where
   arbitrary = sized (\i -> sizedGen True varGen (i `min` 4))

instance CoArbitrary SLogic where
   coarbitrary = foldLogic 
      (var, bin 1, bin 2, bin 3, bin 4, un 5, con 6, con 7)
    where
      con       = variant :: Int -> Gen a -> Gen a
      var       = un 0 . coarbitrary . map ord . fromShowString
      un  n a   = con n . a
      bin n a b = con n . a . b