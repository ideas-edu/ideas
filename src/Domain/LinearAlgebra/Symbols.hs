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
module Domain.LinearAlgebra.Symbols () where

import Domain.Math.Expr.Conversion
import Domain.Math.Expr.Symbolic
import Domain.Math.Simplification
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.Vector
import Control.Monad
import qualified Text.OpenMath.Dictionary.Linalg2 as Linalg2
import Common.Rewriting.Term

vectorSymbol, matrixSymbol, matrixrowSymbol :: Symbol
vectorSymbol    = toSymbol Linalg2.vectorSymbol
matrixSymbol    = toSymbol Linalg2.matrixSymbol
matrixrowSymbol = toSymbol Linalg2.matrixrowSymbol

-------------------------------------------------------
-- Conversion to the Expr data type

instance IsTerm a => IsTerm (Matrix a) where
   toTerm = 
      let f = function matrixrowSymbol . map toTerm
      in function matrixSymbol . map f . rows
   fromTerm a = do
      rs  <- isSymbol matrixSymbol a
      xss <- mapM (isSymbol matrixrowSymbol) rs
      yss <- mapM (mapM fromTerm) xss
      guard (isRectangular yss)
      return (makeMatrix yss)

instance IsTerm a => IsTerm (Vector a) where
   toTerm = function vectorSymbol . map toTerm . toList
   fromTerm a = do
      xs <- isSymbol vectorSymbol a
      ys <- mapM fromTerm xs
      return (fromList ys)
      
instance IsTerm a => IsTerm (VectorSpace a) where
   toTerm = toTerm . vectors
   fromTerm a = do
      xs <- fromTerm a
      guard (sameDimension xs)
      return (makeVectorSpace xs)

-------------------------------------------------------
-- Simplification

instance Simplify a => Simplify (Matrix a) where
   simplify = fmap simplify

instance Simplify a => Simplify (Vector a) where
   simplify = fmap simplify
   
instance Simplify a => Simplify (VectorSpace a) where
   simplify = fmap simplify