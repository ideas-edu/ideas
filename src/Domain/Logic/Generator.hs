module Domain.Logic.Generator 
   ( generateLogic, generateLogicWith, suitableLogic, suitableLogicWith, makeSuitable
   , LogicGenConfig(..), defaultConfig
   ) where

import Domain.Logic.Formula
import Control.Monad
import Test.QuickCheck hiding (defaultConfig)
import System.Random

suitableLogic :: (Logic -> Bool) -> StdGen -> Logic
suitableLogic p = makeSuitable p generateLogic
            
suitableLogicWith :: (Logic -> Bool) ->  LogicGenConfig -> StdGen -> Logic
suitableLogicWith p config = makeSuitable p (generateLogicWith config)

makeSuitable :: (Logic -> Bool) -> (StdGen -> Logic) -> StdGen -> Logic
makeSuitable p f gen
   | p logic   = logic 
   | otherwise = makeSuitable p f (fst $ split gen)
 where logic = f gen 

generateLogic :: StdGen -> Logic
generateLogic = generateLogicWith defaultConfig
   
generateLogicWith :: LogicGenConfig -> StdGen -> Logic
generateLogicWith config stdGen =
   generate 0 stdGen (arbLogic config)
   
data LogicGenConfig = LogicGenConfig
   { maxDepth      :: Int
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
   { maxDepth      = 4
   , differentVars = 3
   , freqConstant  = 1
   , freqVariable  = 2
   , freqImpl      = 2
   , freqEquiv     = 2
   , freqAnd       = 3
   , freqOr        = 3
   , freqNot       = 3
   }

freqLeaf :: LogicGenConfig -> Int
freqLeaf config = freqConstant config + freqVariable config

arbLogic :: LogicGenConfig -> Gen Logic
arbLogic config
   | maxDepth config == 0 = arbLogicLeaf config
   | otherwise            = arbLogicBin  config

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
   rec   = arbLogic config {maxDepth = maxDepth config - 1}
   op1 f = liftM  f rec
   op2 f = liftM2 f rec rec

variableList :: [String]
variableList = ["p", "q", "r", "s", "t"] ++ [ "x" ++ show n | n <- [0..] ]