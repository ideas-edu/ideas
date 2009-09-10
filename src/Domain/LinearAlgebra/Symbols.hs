module Domain.LinearAlgebra.Symbols 
   ( -- linalg2
     matrixSymbol, matrixrowSymbol, vectorSymbol
   ) where

import Domain.Math.Expr.Conversion
import Domain.Math.Expr.Symbolic
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.Vector
import Control.Monad
import Text.OpenMath.Dictionary.Linalg2

-------------------------------------------------------
-- Conversion to the Expr data type

instance IsExpr a => IsExpr (Matrix a) where
   toExpr = 
      let f = function matrixrowSymbol . map toExpr
      in function matrixSymbol . map f . rows
   fromExpr a = do
      rs  <- isSymbol matrixSymbol a
      xss <- mapM (isSymbol matrixrowSymbol) rs
      yss <- mapM (mapM fromExpr) xss
      guard (isRectangular yss)
      return (makeMatrix yss)
      
instance IsExpr a => IsExpr (Vector a) where
   toExpr = function vectorSymbol . map toExpr . toList
   fromExpr expr = do
      xs <- isSymbol vectorSymbol expr
      ys <- mapM fromExpr xs
      return (fromList ys)