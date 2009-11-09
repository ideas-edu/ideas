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
module Domain.Math.Polynomial.QuadraticFormula 
   ( abcStrategy, abcFormula 
   ) where

import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Data.OrList
import Common.Context
import Common.View hiding (simplify)
import Common.Transformation
import Common.Traversable
import Domain.Math.Polynomial.Views
import Domain.Math.Numeric.Views
import Control.Monad
import Data.Maybe
import Text.OpenMath.Object
import Domain.Math.Simplification
import Common.Utils
import Common.Strategy hiding (fail)

abcFormula :: Rule (OrList (Equation Expr))
abcFormula = makeSimpleRule "abc formula" $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   let discr = sqrt (fromRational (b*b - 4 * a * c))
   case compare discr 0 of
      LT -> return false
      EQ -> return $ return $ 
         Var x :==: (-fromRational b) / (2 * fromRational a)
      GT -> return $ orList
         [ Var x :==: (-fromRational b + discr) / (2 * fromRational a)
         , Var x :==: (-fromRational b - discr) / (2 * fromRational a)
         ]
         
abcStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
abcStrategy = label "abc formula" $
   liftRuleIn onceV findABC <*> 
   liftRuleIn onceV findDiscriminant <*>
   quadraticFormula
          
onceV :: View (Context (OrList (Equation Expr))) 
              (Context (Equation Expr), (OrList (Equation Expr), OrList (Equation Expr)))
onceV = makeView f g
 where
   f ce = do 
      xs <- disjunctions (fromContext ce)
      let myView = polyNormalForm rationalView >>> second quadraticPolyView
          p (lhs :==: rhs) = lhs `belongsTo` myView && rhs == 0
      case break p xs of
         (xs, y:ys) -> do 
            return (makeContext (getEnvironment ce) y, (orList xs, orList ys))
         _ -> Nothing
   g (ce, (xs, ys)) = 
      let new = xs \/ return (fromContext ce) \/ ys
      in makeContext (getEnvironment ce) new
         
---------------------------------------------------------------------
-- Quadratic Formula with intermediate steps

findABC :: Rule (Context (Equation Expr))
findABC = minorRule $ makeSimpleRule "find abc" $ withCM $ \eq@(lhs :==: rhs) -> do
   guard (rhs == 0)
   let myView = polyNormalForm rationalView >>> second quadraticPolyView
   (_, (a, b, c)) <- matchM myView lhs
   addToClipboard "a" (fromRational a)
   addToClipboard "b" (fromRational b)
   addToClipboard "c" (fromRational c)
   return eq

findDiscriminant :: Rule (Context (Equation Expr))
findDiscriminant = minorRule $ makeSimpleRule "find discriminant" $ withCM $ \eq -> do
   [a, b, c] <- lookupListClipboard ["a", "b", "c"]
   let discr = simplify (b*b - 4*a*c)
   addToClipboard "D" discr
   return eq

quadraticFormula :: Rule (Context (OrList (Equation Expr)))
quadraticFormula = makeSimpleRule "abc formula" $ withCM $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, _) <- matchM (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   [a, b, discr] <- lookupListClipboard ["a", "b", "D"]
   case compare discr 0 of
      LT -> return false
      EQ -> return $ return $ 
         Var x :==: (-b) / (2*a)
      GT -> return $ orList
         [ Var x :==: (-b + sqrt discr) / (2*a)
         , Var x :==: (-b - sqrt discr) / (2*a)
         ]

---------------------------------------------------------------------
-- Clipboard variable helper functions

newtype ExprVar a = ExprVar (Var OMOBJ)

exprVar :: (Show a, IsExpr a) => String -> a -> ExprVar a
exprVar s a = 
   let omobj = toOMOBJ (toExpr a)
       showF = show  . fromOMOBJ
       readF = either (fail . show) (return . toOMOBJ) . parseExpr
   in ExprVar (makeVar showF readF s omobj)

lookupExprVar :: IsExpr a => ExprVar a -> ContextMonad a
lookupExprVar (ExprVar var) = do  
   omobj <- readVar var
   maybeCM $ fromExpr (fromOMOBJ omobj)

changeExprVar :: IsExpr a => ExprVar a -> (a -> a) -> ContextMonad ()
changeExprVar (ExprVar var) f = 
   let safe f a = fromMaybe a (f a)
       g = fmap (toOMOBJ . toExpr . f) . fromExpr . fromOMOBJ
   in modifyVar var (safe g)

--------
clipboard :: ExprVar (Equations Expr)
clipboard = exprVar "clipboard" []
   
addToClipboard :: String -> Expr -> ContextMonad ()
addToClipboard s a = changeExprVar clipboard ((Var s :==: toExpr a):)

lookupClipboard :: String -> ContextMonad Expr
lookupClipboard s = do 
   eqs <- lookupExprVar clipboard
   let f eqs = safeHead [ e | Var y :==: e <- eqs, s==y ] 
   maybeCM (f eqs)
   
lookupListClipboard :: [String] -> ContextMonad [Expr]
lookupListClipboard = mapM lookupClipboard