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
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Text.OpenMath.Object
import Common.View
import Control.Monad
import Common.Rewriting.Term (Symbol)
import Data.Maybe
import Data.List
import Common.Rewriting
import qualified Common.Rewriting.Term as Term

instance Rewrite a => Rewrite (Equation a)

termToExpr :: Term.Term -> Expr
termToExpr = maybe undefined id . fromTerm

-----------------------------------------------------------------------
-- Type class for expressions

toExpr :: IsTerm a => a -> Expr
toExpr = termToExpr . toTerm

fromExpr :: (MonadPlus m, IsTerm a) => Expr -> m a
fromExpr = maybe (fail "") return . fromTerm . toTerm

exprView :: IsTerm a => View Expr a
exprView = makeView fromExpr toExpr

instance IsTerm a => IsTerm [a] where
   toTerm = function listSymbol . map toTerm
   fromTerm expr = isSymbol listSymbol expr >>= mapM fromTerm

-------------------------------------------------------------
-- Conversions to the Expr data type

instance IsTerm a => IsTerm (Equation a) where
   toTerm = toTerm . build equationView
   fromTerm expr = fromTerm expr >>= matchM equationView

instance IsTerm a => IsTerm (Relation a) where
   toTerm p = 
      let op  = relationType p
          sym = fromMaybe (toSymbol (show op)) (lookup op relationSymbols)
      in binary sym (toTerm (leftHandSide p)) (toTerm (rightHandSide p))
   fromTerm expr = 
      let f (relType, s) = do
             (e1, e2) <- isBinary s expr
             liftM2 (makeType relType) (fromTerm e1) (fromTerm e2)
      in msum (map f relationSymbols) 

relationSymbols :: [(RelationType, Symbol)]
relationSymbols =
   [ (EqualTo, eqSymbol), (NotEqualTo, neqSymbol), (LessThan, ltSymbol)
   , (GreaterThan, gtSymbol), (LessThanOrEqualTo, leqSymbol)
   , (GreaterThanOrEqualTo, geqSymbol), (Approximately, approxSymbol)
   ]

instance IsTerm a => IsTerm (OrList a) where
   toTerm = toTerm . build orView
   fromTerm expr = fromTerm expr >>= matchM orView

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