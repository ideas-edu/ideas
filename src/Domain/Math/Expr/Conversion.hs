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
   ( toOMOBJ, fromOMOBJ
   , toExpr, fromExpr, exprView
   ) where

import Domain.Math.Expr.Data
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols
import Text.OpenMath.Object
import Common.View
import Control.Monad
import Data.Maybe
import Data.List
import Common.Rewriting

-----------------------------------------------------------------------
-- Type class for expressions

toExpr :: IsTerm a => a -> Expr
toExpr a =
   case fromTerm (toTerm a) of
      Just expr -> expr
      Nothing   -> error "Invalid term"

fromExpr :: (MonadPlus m, IsTerm a) => Expr -> m a
fromExpr = fromTerm . toTerm

exprView :: IsTerm a => View Expr a
exprView = makeView fromExpr toExpr

instance IsTerm a => IsTerm [a] where
   toTerm = function listSymbol . map toTerm
   fromTerm a = isSymbol listSymbol a >>= mapM fromTerm

-------------------------------------------------------------
-- Symbol Conversion to/from OpenMath

toOMOBJ :: Expr -> OMOBJ
toOMOBJ (Var x)    = OMV x
toOMOBJ (Nat n)    = OMI n
toOMOBJ (Number a) = OMF a
toOMOBJ expr =
   case getFunction expr of
      Just (s, []) 
         | s == listSymbol -> 
              OMA [OMS (fromSymbol s)]
         | otherwise -> 
              OMS (fromSymbol s)  
      Just (s, [Var x, e]) | s == lambdaSymbol -> 
         OMBIND (OMS (fromSymbol lambdaSymbol)) [x] (toOMOBJ e)
      Just (s, xs) -> 
         OMA (OMS (fromSymbol s):map toOMOBJ xs)
      Nothing -> 
         error $ "toOMOBJ: " ++ show expr

fromOMOBJ :: OMOBJ -> Expr
fromOMOBJ omobj =
   case omobj of
      OMI n -> fromInteger n
      OMF a -> fromDouble a
      OMV x -> Var x
      OMS s -> symbol (toSymbol s)
      OMA (OMS s:xs) -> function (toSymbol s) (map fromOMOBJ xs)
      OMBIND (OMS s) [x] body ->
         binary s (Var x) (fromOMOBJ body)
      _ -> symbol $ toSymbol $ show omobj