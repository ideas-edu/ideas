{-# OPTIONS -fglasgow-exts #-}
module OpenMath.StrategyTable where

import Common.Context
import Common.Assignment
import Common.Unification
import Common.Strategy
import Common.Transformation
import Domain.LinearAlgebra (reduceMatrixAssignment, solveSystemAssignment, solveGramSchmidt, MySqrt)
import qualified Domain.LinearAlgebra as MySqrt
import Domain.LinearAlgebra (Matrix, rows, matrix, makeMatrix, MatrixInContext, 
                             EqsInContext(..), equations, LinearExpr, getConstant, coefficientOf, var)
import Domain.LinearAlgebra.Equation (Equation, getLHS, getRHS)
import Domain.LinearAlgebra.Vector (Vector, toList, fromList)
import qualified Domain.LinearAlgebra.Equation as LA
import OpenMath.ObjectParser
import Control.Monad

type StrategyID = String

versionNr :: String
versionNr = "0.2.7"

oneliner :: String -> String
oneliner = unwords . concatMap words . lines

defaultURL :: Bool -> String
defaultURL b = "http://ideas.cs.uu.nl/cgi-bin/laservice.cgi?" ++ (if b then "mode=html&" else "") ++ "input="

data ExprAssignment = forall a . IsExpr a => ExprAssignment (Assignment (Context a))

data StrategyEntry = Entry 
   { strategyNr     :: String
   , exprAssignment :: ExprAssignment
   , functions      :: [String]
   , examples       :: [Expr]
   }
 
entry :: IsExpr a => String -> Assignment (Context a) -> [String] -> [a] -> StrategyEntry
entry nr a fs ex = Entry nr (ExprAssignment a) fs (map toExpr ex)

strategyTable :: [StrategyEntry]
strategyTable =
   [ entry "2.5" reduceMatrixAssignment 
        ["toReducedEchelon"]
        [makeMatrix [[6, 3], [2, 4]], makeMatrix [[0,1,1,1], [1,2,3,2], [3,1,1,3]]]
   , let (x1, x2, x3, x4) = (var "x1", var "x2", var "x3", var "x4") in
     entry "1.7" solveSystemAssignment  
        ["generalSolutionLinearSystem", "systemToEchelonWithEEO", "backSubstitutionSimple"]
        [ [x2 + 2 * x3 LA.:==: 1, x1 + 2 * x2 + 3 * x3 LA.:==: 2, 3 * x1 + x2 + x3 LA.:==: 3]
        , [x1 + 2 * x2 + 3 * x3 - x4 LA.:==: 0, 2 * x1 + 3 * x2 - x3 + 3 * x4 LA.:==: 0, 4 * x1 + 6 * x2 + x3 + 2 * x4 LA.:==: 0 ]
        , [ x1 + x2 - 2*x3 LA.:==: 0, 2*x1 + x2 - 3*x3 LA.:==: 0, 4*x1 - 2*x2 - 2*x3 LA.:==: 0, 6*x1 - x2 - 5*x3 LA.:==: 0, 7*x1 - 3*x2 - 4*x3 LA.:==: 1]
        ]
   , entry "8.6" solveGramSchmidt       
        ["gramSchmidt"]
        [[fromList [1,1,1,1], fromList [3,3,1,1], fromList [7,9,3,5]]]
   ]

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