-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.Logic.Generator 
   ( generateLogic, generateLogicWith
   , LogicGenConfig(..), defaultConfig
   , equalLogicA
   ) where

import Domain.Logic.Formula
import Control.Monad
import Data.Char
import Test.QuickCheck hiding (defaultConfig)
import Common.Rewriting
import Common.Uniplate

instance Rewrite Logic where
   operators = logicOperators

-- | Equality modulo associativity of operators
equalLogicA:: Logic -> Logic -> Bool
equalLogicA = equalWith operators



generateLogic :: Gen Logic
generateLogic = generateLogicWith defaultConfig
   
generateLogicWith :: LogicGenConfig -> Gen Logic
generateLogicWith config = arbLogic config >>= preventSameVar config >>= (return . removePartsInDNF)
   
data LogicGenConfig = LogicGenConfig
   { maxSize       :: Int
   , differentVars :: Int
   , freqConstant  :: Int
   , freqVariable  :: Int
   , freqImpl      :: Int
   , freqEquiv     :: Int
   , freqAnd       :: Int
   , freqOr        :: Int
   , freqNot       :: Int
   }
 deriving Show

defaultConfig :: LogicGenConfig
defaultConfig = LogicGenConfig
   { maxSize       = 4
   , differentVars = 3
   , freqConstant  = 0 -- 1
   , freqVariable  = 4
   , freqImpl      = 4
   , freqEquiv     = 2
   , freqAnd       = 6
   , freqOr        = 6
   , freqNot       = 6
   }

freqLeaf :: LogicGenConfig -> Int
freqLeaf config = freqConstant config + freqVariable config

arbLogic :: LogicGenConfig -> Gen Logic
arbLogic config
   | maxSize config == 0 = arbLogicLeaf config
   | otherwise           = arbLogicBin  config

arbLogicLeaf :: LogicGenConfig -> Gen Logic
arbLogicLeaf config = frequency
   [ (freqConstant config, oneof $ map return [F, T])
   , (freqVariable config, oneof [ return (Var x) | x <- take (differentVars config) variableList])
   ]

arbLogicBin :: LogicGenConfig -> Gen Logic
arbLogicBin config = frequency
   [ (freqLeaf  config, arbLogicLeaf config)
   , (freqImpl  config, op2 (:->:))
   , (freqEquiv config, op2 (:<->:))
   , (freqAnd   config, op2 (:&&:))
   , (freqOr    config, op2 (:||:))
   , (freqNot   config, op1 Not)
   ]
 where
   rec   = arbLogic config {maxSize = maxSize config `div` 2}
   op1 f = liftM  f rec
   op2 f = liftM2 f rec rec

preventSameVar :: LogicGenConfig -> Logic -> Gen Logic
preventSameVar config = rec 
 where
   rec p = case uniplate p of
              ([Var x, Var y], f) | x==y -> do
                 z <- oneof [ return z
                            | z <- take (differentVars config) variableList
                            , z /= x
                            ]
                 return (f [Var x, Var z])
              (cs, f) -> liftM f (mapM rec cs)

removePartsInDNF :: Logic -> Logic
removePartsInDNF = buildOr . filter p1 . disjunctions
 where
   buildOr [] = T
   buildOr xs = foldl1 (:||:) xs
   
   p1 = all (not . p2) . conjunctions
   p2 (Var _) = True
   p2 (Not q) = p2 q
   p2 T       = True
   p2 F       = True
   p2 _       = False

variableList :: [String]
variableList = ["p", "q", "r", "s", "t"] ++ [ "x" ++ show n | n <- [0..] ]

-----------------------------------------------------------
--- QuickCheck generator

instance Arbitrary Logic where
   arbitrary = sized $ \n -> arbLogic defaultConfig {maxSize = n}
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