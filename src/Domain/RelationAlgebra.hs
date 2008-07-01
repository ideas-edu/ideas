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
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.RelationAlgebra 
   ( module Domain.RelationAlgebra.Formula
   , module Domain.RelationAlgebra.Rules
   , module Domain.RelationAlgebra.Exercises
   , module Domain.RelationAlgebra.Strategies
   , module Domain.RelationAlgebra.Generator
   , module Domain.RelationAlgebra.Parser
   ) where
   
import Domain.RelationAlgebra.Formula
import Domain.RelationAlgebra.Rules
import Domain.RelationAlgebra.Exercises
import Domain.RelationAlgebra.Strategies
import Domain.RelationAlgebra.Generator
import Domain.RelationAlgebra.Parser
import Domain.RelationAlgebra.Equivalence

import Test.QuickCheck 
import System.Random
import Data.List
import Common.Apply
import Common.Context
import Control.Monad

nrpairs = 2000 -- 20000

repeatM :: Monad m => m a -> m [a]
repeatM m = liftM2 (:) m (repeatM m)

pairs :: [(RelAlg, RelAlg)]
pairs = take nrpairs $ generate 100 (mkStdGen 280578) (repeatM arbitrary)
 
precision :: IO ()
precision = do 
   let f (x, y) = probablyEqualWithG (mkStdGen 28) x y
       ms   = map f pairs
       freq = map g $ group $ sort ms
       is   = [ n | (Just n, _)  <- freq ]
       g xs@(x:_) = (x, length xs)
       h n = let score = sum [ i | (Just m, i) <- freq, m <= n ]
             in putStrLn $ show n ++ ": " ++ showPerc (nrpairs - score - dif)
       troubles = [ (norm p, norm q) | (Nothing, (p, q)) <- zip ms pairs ] 
       len = length unknown
       dif = length troubles - len
       unknown  = -- map (\(a,b) -> (a, b, isEquivalent a b)) $ 
                  filter (\(a,b) -> a /= b) troubles
   putStrLn $ map (maybe '!' (const '.')) ms
   mapM_ h is
   
   putStrLn $ unlines $ map show unknown
   putStrLn $ "(" ++ show len ++ " unknown)"

showPerc :: Int -> String
showPerc n = show (fromIntegral (100*n)/fromIntegral nrpairs) ++ "%"

norm :: RelAlg -> RelAlg
norm = fromContext . applyD toCNF . inContext

pair1 = ((Not (Inv (Var "q")) :&&: Not (Inv (Var "s"))) :&&: Inv (Var "s") :.: Inv (Var "q"),E)
pair2 = ((Var "s" :&&: (E :+: Not (Var "r")) :.: Inv (Var "r") :&&: ((Not (Var "s") :.: Var "q") :||: (Not (Var "s") :.: Var "s"))) :.: (Var "s" :+: Inv (Var "r") :.: (Inv (Var "s") :+: Inv (Var "r")) :&&: Inv (Var "q")),E)
pair3 = ((Not (Var "q") :||: Not (Var "s")) :||: ((Inv (Var "r") :+: E :.: Inv (Var "q")) :||: (Not (Var "q") :||: Var "s")),U)

test1 = uncurry isEquivalent pair1
test2 = uncurry isEquivalent pair2
test3 = uncurry isEquivalent pair3