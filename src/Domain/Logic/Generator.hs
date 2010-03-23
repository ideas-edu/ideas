{-# LANGUAGE TypeSynonymInstances #-}
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
module Domain.Logic.Generator
   ( generateLogic, generateLevel, equalLogicA, Level(..)
   ) where

import Domain.Logic.Formula
import Control.Monad
import Data.Char
import Test.QuickCheck hiding (defaultConfig)
import Common.Rewriting
import Common.Uniplate

import qualified Common.Rewriting.Term as Term

-------------------------------------------------------------
-- Code that doesn't belong here, but the arbitrary instance
-- is needed for the Rewrite instance.

instance Different (Logic a) where
   different = (T, F)

instance IsTerm a => IsTerm (Logic a) where
   toTerm = foldLogic (toTerm, tBin "->", tBin "<->", tBin "&&", tBin "||", tUn "~", Term.Con "T", Term.Con "F")
   
   fromTerm (Term.Con "T") = return T
   fromTerm (Term.Con "F") = return F
   fromTerm (Term.App (Term.Con "~") a) = liftM Not (fromTerm a)
   fromTerm term = msum 
      [ iBin "->" (:->:) term, iBin "<->" (:<->:) term
      , iBin "&&" (:&&:) term, iBin "||" (:||:) term
      , liftM Var (fromTerm term)
      ]
   
   
tBin s a b = Term.App (Term.App (Term.Con s) a) b
tUn s a = Term.App (Term.Con s) a

iBin s f term = do 
   (a, b) <- Term.isBin s term
   p <- fromTerm a
   q <- fromTerm b
   return (f p q)


instance Rewrite SLogic where
   operators = logicOperators

-- | Equality modulo associativity of operators
equalLogicA:: SLogic -> SLogic -> Bool
equalLogicA = equalWith operators

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
   n  <- return 4 -- oneof [return 4, return 8]
   p0 <- sizedGen False varGen n
   p1 <- preventSameVar varList p0
   return (removePartsInDNF p1)

-- Use the propositions with 7-18 steps
difficultGenerator :: Gen SLogic
difficultGenerator = do
   let vars = "s" : varList
   n  <- return 4 -- oneof [return 4, return 8]
   p0 <- sizedGen False (oneof $ map return vars) n
   p1 <- preventSameVar vars p0
   return (removePartsInDNF p1)

varList :: [String]
varList = ["p", "q", "r"]

varGen :: Gen String
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
preventSameVar xs = transformM $ \p -> 
   case uniplate p of
      ([Var a, Var b], f) | a==b -> do
         c <- oneof $ map return $ filter (/=a) xs
         return $ f [Var a, Var c]
      _ -> return p

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
   arbitrary = sized (sizedGen True varGen)
   coarbitrary logic = 
      case logic of
         Var x     -> variant 0 . coarbitrary (map ord x)
         p :->: q  -> variant 1 . coarbitrary p . coarbitrary q
         p :<->: q -> variant 2 . coarbitrary p . coarbitrary q
         p :&&: q  -> variant 3 . coarbitrary p . coarbitrary q
         p :||: q  -> variant 4 . coarbitrary p . coarbitrary q
         Not p     -> variant 5 . coarbitrary p
         T         -> variant 6  
         F         -> variant 7