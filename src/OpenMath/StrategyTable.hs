{-# OPTIONS -fglasgow-exts #-}
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
module OpenMath.StrategyTable where

import Common.Context
import Common.Exercise
import Common.Unification
import Common.Strategy
import Common.Transformation
import Common.Utils (Some(..))
import Domain.LinearAlgebra (reduceMatrixExercise, solveSystemExercise, solveGramSchmidt, MySqrt, solveSystemWithMatrixExercise)
import qualified Domain.LinearAlgebra as MySqrt
import Domain.LinearAlgebra (Matrix, rows, matrix, makeMatrix, MatrixInContext, 
                             EqsInContext(..), equations, LinearExpr, getConstant, coefficientOf, var, ShowRational(..))
import Domain.LinearAlgebra.Equation (Equation, getLHS, getRHS)
import Domain.LinearAlgebra.Vector (Vector, toList, fromList)
import qualified Domain.LinearAlgebra.Equation as LA
import OpenMath.ObjectParser
import Control.Monad

type StrategyID = String

versionNr :: String
versionNr = "0.2.10"

oneliner :: String -> String
oneliner = unwords . concatMap words . lines

defaultURL :: Bool -> String
defaultURL b = "http://ideas.cs.uu.nl/cgi-bin/laservice.cgi?" ++ (if b then "mode=html&" else "") ++ "input="

data ExprExercise a = IsExpr a => ExprExercise (Exercise (Context a))

data StrategyEntry = Entry 
   { strategyNr   :: String
   , exprExercise :: Some ExprExercise
   , functions    :: [String]
   , examples     :: [Expr]
   }
 
entry :: IsExpr a => String -> Exercise (Context a) -> [String] -> [a] -> StrategyEntry
entry nr a fs ex = Entry nr (Some (ExprExercise a)) fs (map toExpr ex)

strategyTable :: [StrategyEntry]
strategyTable =
   [ entry "2.5" reduceMatrixExercise
        ["toReducedEchelon"]
        [makeMatrix [[6, 3], [2, 4]], makeMatrix [[0,1,1,1], [1,2,3,2], [3,1,1,3]]]
   , entry "1.7" solveSystemExercise
        ["generalSolutionLinearSystem", "systemToEchelonWithEEO", "backSubstitutionSimple"]
        [sys1, sys2, sys3]
   , entry "2.6" solveSystemWithMatrixExercise
        ["generalSolutionSystemWithMatrix"]
        (map Left [sys1, sys2, sys3])
   , entry "8.6" solveGramSchmidt       
        ["gramSchmidt"]
        [[fromList [1,1,1,1], fromList [3,3,1,1], fromList [7,9,3,5]]]
   ]
 where
   (x1, x2, x3, x4) = (var "x1", var "x2", var "x3", var "x4")
   sys1 = [x2 + 2 * x3 LA.:==: 1, x1 + 2 * x2 + 3 * x3 LA.:==: 2, 3 * x1 + x2 + x3 LA.:==: 3]
   sys2 = [x1 + 2 * x2 + 3 * x3 - x4 LA.:==: 0, 2 * x1 + 3 * x2 - x3 + 3 * x4 LA.:==: 0, 4 * x1 + 6 * x2 + x3 + 2 * x4 LA.:==: 0 ]
   sys3 = [ x1 + x2 - 2*x3 LA.:==: 0, 2*x1 + x2 - 3*x3 LA.:==: 0, 4*x1 - 2*x2 - 2*x3 LA.:==: 0, 6*x1 - x2 - 5*x3 LA.:==: 0, 7*x1 - 3*x2 - 4*x3 LA.:==: 1]

instance IsExpr a => IsExpr (Matrix a) where
   toExpr   = Matrix . map (map toExpr) . rows
   fromExpr (Matrix xs) = do 
      rows <- mapM (mapM fromExpr) xs
      return $ makeMatrix rows
   fromExpr _ = Nothing
   
instance (Fractional a, IsExpr a) => IsExpr (LinearExpr a) where
   toExpr x =
      let op s e = (toExpr (coefficientOf s x) :*: Var s) :+: e
      in foldr op (toExpr $ getConstant x) (getVarsList x)
   fromExpr e =
      case e of
         Con n    -> Just (fromIntegral n)
         Var s    -> Just (var s)
         Negate x -> liftM negate (fromExpr x)
         x :+: y  -> binop (+) x y
         x :-: y  -> binop (-) x y
         x :*: y  -> guard (isCon x || isCon y) >> binop (*) x y
         x :/: y  -> guard (isCon y)            >> binop (/) x y
         _        -> Nothing
    where
      binop op x y = liftM2 op (fromExpr x) (fromExpr y)
      isCon e =
         case e of
            Con _    -> True
            Negate x -> isCon x
            x :+: y  -> isCon x && isCon y
            x :-: y  -> isCon x && isCon y
            x :*: y  -> isCon x && isCon y
            x :/: y  -> isCon x && isCon y
            _        -> False

instance IsExpr a => IsExpr (Equation a) where
   toExpr eq = toExpr (getLHS eq) :==: toExpr (getRHS eq)
   fromExpr (e1 :==: e2) = do
      x <- fromExpr e1
      y <- fromExpr e2
      return (x LA.:==: y) 
   fromExpr _ = Nothing
   
instance IsExpr a => IsExpr (Vector a) where
   toExpr = List . map toExpr . toList
   fromExpr (List es) = liftM fromList (mapM fromExpr es)
   fromExpr _ = Nothing
   
instance IsExpr MySqrt where 
   toExpr (MySqrt.Con c)    = toExpr c
   toExpr (MySqrt.Sqrt a n)
      | a == 1    = e
      | otherwise = toExpr a :*: e
    where 
      e = Sqrt $ Con $ fromIntegral n
   fromExpr e = -- unchecked!
      case e of
         Con n    -> Just (fromIntegral n)
         Negate x -> liftM negate (fromExpr x)
         x :+: y  -> binop (+) x y
         x :-: y  -> binop (-) x y
         x :*: y  -> binop (*) x y
         x :/: y  -> binop (/) x y
         Sqrt x   -> liftM sqrt (fromExpr x)
         _        -> Nothing
    where
      binop op x y = liftM2 op (fromExpr x) (fromExpr y)
      
instance IsExpr ShowRational where
   toExpr (ShowRational r) = toExpr r
   fromExpr = fmap ShowRational . fromExpr