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
   ( IsExpr(..), toOMOBJ, fromOMOBJ, exprToSLogic
   ) where

import Domain.Math.Expr.Data
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import qualified Domain.Logic.Formula as Logic
import Domain.Logic.Formula (Logic((:||:), (:&&:), (:<->:), (:->:)))
import Text.OpenMath.Object
import Text.OpenMath.Symbol (extraSymbol)
import Common.View
import Control.Monad
import Common.Utils (ShowString(..))
import Common.Traversable (switch)
import Data.Maybe
import Data.List
import Common.Rewriting
import qualified Common.Rewriting.Term as Term

instance Rewrite a => Rewrite (Equation a)

instance IsTerm a => IsTerm (Equation a) where
   toTerm (a :==: b) = Term.binary (show eqSymbol) (toTerm a) (toTerm b)
   fromTerm (Term.App (Term.App (Term.Con s) a) b)
      | read s == eqSymbol = liftM2 (:==:) (fromTerm a) (fromTerm b)
   fromTerm _ = Nothing

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
   toExpr = toExpr . build equationView
   fromExpr expr = fromExpr expr >>= matchM equationView
   
instance IsExpr a => IsExpr (Relation a) where
   toExpr p = 
      let op  = relationType p
          sym = fromMaybe (extraSymbol (show op)) (lookup op relationSymbols)
      in binary sym (toExpr (leftHandSide p)) (toExpr (rightHandSide p))
   fromExpr expr = 
      let f (relType, s) = do
             (e1, e2) <- isBinary s expr
             liftM2 (makeType relType) (fromExpr e1) (fromExpr e2)
      in msum (map f relationSymbols) 

relationSymbols :: [(RelationType, Symbol)]
relationSymbols = 
   [ (EqualTo, eqSymbol), (NotEqualTo, neqSymbol), (LessThan, ltSymbol)
   , (GreaterThan, gtSymbol), (LessThanOrEqualTo, leqSymbol)
   , (GreaterThanOrEqualTo, geqSymbol), (Approximately, approxSymbol)
   ]
   
instance IsExpr a => IsExpr (OrList a) where
   toExpr = toExpr . build orView
   fromExpr expr = fromExpr expr >>= matchM orView
         
instance IsExpr a => IsExpr (Logic a) where
   toExpr logic = 
      case logic of
         Logic.T     -> symbol trueSymbol
         Logic.F     -> symbol falseSymbol
         Logic.Var p -> toExpr p
         Logic.Not p -> unary  notSymbol        (toExpr p)
         p :||:  q   -> binary orSymbol         (toExpr p) (toExpr q)
         p :&&:  q   -> binary andSymbol        (toExpr p) (toExpr q)
         p :<->: q   -> binary equivalentSymbol (toExpr p) (toExpr q)
         p :->:  q   -> binary impliesSymbol    (toExpr p) (toExpr q)
   fromExpr expr = msum
      [ guard (isConst trueSymbol expr)  >> return Logic.T
      , guard (isConst falseSymbol expr) >> return Logic.F
      , do a <- isUnary notSymbol expr
           liftM Logic.Not (fromExpr a)
      , do (a, b) <- isBinary orSymbol expr
           liftM2 (:||:) (fromExpr a) (fromExpr b)
      , do (a, b) <- isBinary andSymbol expr
           liftM2 (:&&:) (fromExpr a) (fromExpr b)
      , do (a, b) <- isBinary equivalentSymbol expr
           liftM2 (:<->:) (fromExpr a) (fromExpr b)
      , do (a, b) <- isBinary impliesSymbol expr
           liftM2 (:->:) (fromExpr a) (fromExpr b)
      , liftM Logic.Var (fromExpr expr)
      ]

exprToSLogic :: MonadPlus m => Expr -> m Logic.SLogic
exprToSLogic expr = fromExpr expr >>= switch . fmap f
 where
   f (Var s) = return (ShowString s)
   f expr    = fail (show expr)

-------------------------------------------------------------
-- Symbol Conversion to/from OpenMath

toOMOBJ :: Expr -> OMOBJ
toOMOBJ (Var x)    = OMV x
toOMOBJ (Nat n)    = OMI n
toOMOBJ (Number a) = OMF a
toOMOBJ expr =
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
      OMF a -> fromDouble a
      OMV x -> Var x
      OMS s -> symbol s
      OMA (OMS s:xs) -> function s (map fromOMOBJ xs)
      OMBIND (OMS s) [x] body ->
         binary s (Var x) (fromOMOBJ body)
      _ -> symbol $ Symbol Nothing $ show omobj