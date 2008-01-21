module OpenMath.StrategyTable where

import Common.Assignment
import Common.Strategy
import Common.Transformation
import Domain.LinearAlgebra.Checks (reduceMatrixAssignment)
import Domain.LinearAlgebra
import OpenMath.ObjectParser

type StrategyID = String
type Location   = [Int]

-- not yet used
strategyTable :: [(String, Assignment Expr)]
strategyTable =
   [ ("Gaussian Elimination", exprAssignment reduceMatrixAssignment)
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

exprAssignment :: IsExpr a => Assignment a -> Assignment Expr
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
   }
   
exprLiftPair :: IsExpr a => LiftPair a Expr
exprLiftPair = LiftPair fromExpr (flip const)

changeStrategy :: LiftPair a b -> Strategy a -> Strategy b
changeStrategy = mapStrategy . liftRule 