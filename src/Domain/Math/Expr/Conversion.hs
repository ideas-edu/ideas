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
module Domain.Math.Expr.Conversion 
   ( IsExpr(..), toOMOBJ, fromOMOBJ
   ) where

import Domain.Math.Expr.Data
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols
import Domain.Math.Data.Equation
import Domain.Math.Data.Inequality
import Domain.Math.Data.OrList
import Text.OpenMath.Object
import Text.OpenMath.Symbol (extraSymbol)
import Common.View
import Control.Monad
import Data.Maybe
import Data.List

-----------------------------------------------------------------------
-- Type class for expressions

class IsExpr a where
   toExpr   :: a -> Expr
   fromExpr :: MonadPlus m => Expr -> m a
   exprView :: View Expr a

   -- default definitions
   toExpr   = build exprView
   fromExpr = maybe (fail "not an expression") return . match exprView
   exprView = makeView fromExpr toExpr

instance IsExpr Expr where
   exprView = identity
   
instance IsExpr a => IsExpr [a] where
   toExpr = function listSymbol . map toExpr
   fromExpr expr = isSymbol listSymbol expr >>= mapM fromExpr

instance (IsExpr a, IsExpr b) => IsExpr (Either a b) where
   toExpr = either toExpr toExpr
   fromExpr expr =
      liftM Left  (fromExpr expr) `mplus`
      liftM Right (fromExpr expr)
   
-------------------------------------------------------------
-- Conversions to the Expr data type

instance IsExpr a => IsExpr (Equation a) where
   toExpr (x :==: y) = binary eqSymbol (toExpr x) (toExpr y)
   fromExpr expr = do
      (e1, e2) <- isBinary eqSymbol expr
      liftM2 (:==:) (fromExpr e1) (fromExpr e2)
   
instance IsExpr a => IsExpr (Inequality a) where
   toExpr ineq = 
      let (x, op, y) = toOperator ineq
          f = fromMaybe (extraSymbol (show op)) . (`lookup` inequalitySymbols)
      in binary (f op) (toExpr x) (toExpr y)
   fromExpr expr = 
      let f (op, s) = do
             (e1, e2) <- isBinary s expr
             liftM2 (\x y -> fromOperator (x, op, y)) (fromExpr e1) (fromExpr e2)
      in msum (map f inequalitySymbols)

inequalitySymbols :: [(Operator, Symbol)]
inequalitySymbols = 
   [ (LessThan, ltSymbol), (GreaterThan, gtSymbol), (NotEqualTo, neqSymbol)
   , (LessThanOrEqualTo, leqSymbol), (GreaterThanOrEqualTo, geqSymbol)
   ]

   
instance IsExpr a => IsExpr (OrList a) where
   toExpr ors = 
      case disjunctions ors of
         Just []  -> symbol falseSymbol
         Just [x] -> toExpr x
         Just xs  -> function orSymbol (map toExpr xs)
         Nothing  -> symbol trueSymbol 

   fromExpr expr = do
      xs <- isSymbol orSymbol expr
      ys <- mapM fromExpr xs
      return (orList ys)
    `mplus` do
      guard (isConst falseSymbol expr) >> return false
    `mplus` do
      guard (isConst trueSymbol  expr) >> return true
    `mplus`
      liftM return (fromExpr expr)
      
-------------------------------------------------------------
-- Symbol Conversion to/from OpenMath

toOMOBJ :: Expr -> OMOBJ
toOMOBJ (Var x) = OMV x
toOMOBJ (Nat n) = OMI n
toOMOBJ expr    =
   case getFunction expr of
      Just (s, []) -> 
         OMS s  
      Just (s, [Var x, e]) | s == lambdaSymbol -> 
         OMBIND (OMS lambdaSymbol) [x] (toOMOBJ e)
      Just (s, xs) -> 
         OMA (OMS s:map toOMOBJ xs)
      Nothing -> 
         error $ "toOMOBJ: " ++ show expr

fromOMOBJ :: OMOBJ -> Expr
fromOMOBJ omobj =
   case omobj of
      OMI n -> fromInteger n
      OMV x -> Var x
      OMS s -> symbol s
      OMA (OMS s:xs) -> function s (map fromOMOBJ xs)
      OMBIND (OMS s) [x] body ->
         binary s (Var x) (fromOMOBJ body)
      _ -> symbol $ Symbol Nothing $ show omobj