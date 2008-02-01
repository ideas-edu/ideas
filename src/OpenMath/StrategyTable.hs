{-# OPTIONS -fglasgow-exts #-}
module OpenMath.StrategyTable where

import Common.Assignment
import Common.Unification
import Common.Strategy
import Common.Transformation
import Domain.LinearAlgebra (reduceMatrixAssignment, solveSystemAssignment)
import Domain.LinearAlgebra (Matrix, rows, matrix, inContext, makeMatrix, MatrixInContext, 
                             EqsInContext(..), equations, LinearExpr, getConstant, coefficientOf, var)
import Domain.LinearAlgebra.Equation (Equation, getLHS, getRHS)
import qualified Domain.LinearAlgebra.Equation as LA
import OpenMath.ObjectParser
import Control.Monad

type StrategyID = String
type Location   = [Int]

versionNr :: String
versionNr = "0.2.0"

data ExprAssignment = forall a . IsExpr a => ExprAssignment (Assignment a)

data StrategyEntry = Entry 
   { strategyNr     :: String
   , exprAssignment :: ExprAssignment
   , functions      :: [String]
   }
 
entry :: IsExpr a => String -> Assignment a -> [String] -> StrategyEntry
entry nr a fs = Entry nr (ExprAssignment a) fs

strategyTable :: [StrategyEntry]
strategyTable =
   [ entry "2.5" reduceMatrixAssignment ["toReducedEchelon"]
   , entry "1.7" solveSystemAssignment  ["generalSolutionLinearSystem", "systemToEchelonWithEEO", "backSubstitutionSimple"]
   ]

instance IsExpr a => IsExpr (Matrix a) where
   toExpr   = Matrix . map (map toExpr) . rows
   fromExpr (Matrix xs) = do 
      rows <- mapM (mapM fromExpr) xs
      return $ makeMatrix rows
   fromExpr _ = Nothing
   
instance (Num a, IsExpr a) => IsExpr (MatrixInContext a) where
   toExpr   = toExpr . matrix
   fromExpr = fmap inContext . fromExpr 

instance (Fractional a, IsExpr a) => IsExpr (EqsInContext a) where
   toExpr   = toExpr . equations
   fromExpr = fmap (\x -> EIC x 0) . fromExpr

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

{- exprAssignment :: IsExpr a => Assignment a -> Assignment Expr
exprAssignment a = Assignment
   { shortTitle    = shortTitle a
   , parser        = either (\(doc, ma) -> Left (emptyDoc {- NEEDS FIX -}, fmap toExpr ma)) (Right . toExpr) . parser a
   , prettyPrinter = \e -> case fromExpr e of
                              Just x  -> prettyPrinter a x
                              Nothing -> "<<invalid expression>>"
   , equivalence   = \e1 e2 -> case (fromExpr e1, fromExpr e2) of
                                  (Just x, Just y) -> equivalence a x y
                                  _                -> False
   , equality      = \e1 e2 -> case (fromExpr e1, fromExpr e2) of
                                  (Just x, Just y) -> equality a x y
                                  _                -> False
   , finalProperty = \e1 -> case fromExpr e1 of
                               Just x -> finalProperty a x
                               _      -> False
   , ruleset       = map (liftRule exprLiftPair) (ruleset a)
   , strategy      = changeStrategy exprLiftPair (strategy a)
   , generator     = do x <- generator a 
                        return (toExpr x)
   , suitableTerm  = \e1 -> case fromExpr e1 of
                               Just x -> suitableTerm a x
                               _      -> False
   , configuration = configuration a
   } -}
   
exprLiftPair :: IsExpr a => LiftPair a Expr
exprLiftPair = LiftPair fromExpr (flip const)

changeStrategy :: LiftPair a b -> LabeledStrategy a -> LabeledStrategy b
changeStrategy = mapLabeledStrategy . liftRule