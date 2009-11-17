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
   ( abcFormula, higherSubst, substBackVar
   ) where

import Common.Traversable
import Common.Uniplate
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Data.OrList
import Data.Char
import qualified Data.Map as M
import Common.Context
import Common.View hiding (simplify)
import Common.Transformation
import Common.Traversable
import Domain.Math.Polynomial.Views
import Domain.Math.Numeric.Views
import Text.OpenMath.Object
import Control.Monad
import Data.Maybe
import Domain.Math.Simplification
import Common.Utils
import Common.Strategy hiding (fail)

abcFormula :: Rule (Context (OrList (Equation Expr)))
abcFormula = makeSimpleRule "abc formula" $ withCM $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- matchM (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   addListToClipboard ["a", "b", "c"] (map fromRational [a, b, c])
   let discr = sqrt (fromRational (b*b - 4 * a * c))
   addToClipboard "D" discr
   case compare discr 0 of
      LT -> return false
      EQ -> return $ return $ 
         Var x :==: (-fromRational b) / (2 * fromRational a)
      GT -> return $ orList
         [ Var x :==: (-fromRational b + discr) / (2 * fromRational a)
         , Var x :==: (-fromRational b - discr) / (2 * fromRational a)
         ]

higherSubst :: Rule (Context (Equation Expr))
higherSubst = makeSimpleRule "higher subst" $ withCM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   let myView = polyView >>> second trinomialPolyView
   (x, ((a, n1), (b, n2), (c, n3))) <- matchM myView lhs
   guard (n1 == 0 && n2 > 1 && n3 `mod` n2 == 0 && x /= "p")
   let new = build myView ("p", ((a, 0), (b, 1), (c, n3 `div` n2)))
   addToClipboard "subst" (toExpr (Var "p" :==: Var x .^. fromIntegral n2))
   return (new :==: 0)

substBackVar :: (Crush f, Crush g) => Rule (Context (f (g Expr)))
substBackVar = makeSimpleRule "subst back var" $ withCM $ \a -> do
   expr <- lookupClipboard "subst"
   case fromExpr expr of
      Just (Var p :==: rhs) -> do
         guard (p `elem` concatMap collectVars (concatMap crush (crush a)))
         let new = rhs
         return (fmap (fmap (subst p rhs)) a)
      _ -> fail "no subst in clipboard"
 where
   subst a b (Var c) | a==c = b
   subst a b expr = build (map (subst a b) cs)
    where (cs, build) = uniplate expr

{-
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
         ] -}

---------------------------------------------------------------------
-- Clipboard variable helper functions

newtype ExprVar a = ExprVar (Var OMOBJ)

exprVar :: (Show a, IsExpr a) => String -> a -> ExprVar a
exprVar s a = 
   let omobj = toOMOBJ (toExpr a)
       showF = show . fromOMOBJ
       readF = either (fail . show) (return . toOMOBJ) . parseExpr
   in ExprVar (makeVar showF readF s omobj)

readExprVar :: IsExpr a => ExprVar a -> ContextMonad a
readExprVar (ExprVar var) = do  
   omobj <- readVar var
   maybeCM (fromExpr (fromOMOBJ omobj))

writeExprVar :: IsExpr a => ExprVar a -> a -> ContextMonad ()
writeExprVar v = modifyExprVar v . const

modifyExprVar :: IsExpr a => ExprVar a -> (a -> a) -> ContextMonad ()
modifyExprVar (ExprVar var) f = 
   let safe f a = fromMaybe a (f a)
       g = fmap (toOMOBJ . toExpr . f) . fromExpr . fromOMOBJ
   in modifyVar var (safe g)

--------
newtype Key = Key String deriving (Show, Eq, Ord)

instance (IsExpr k, Ord k, IsExpr a) => IsExpr (M.Map k a) where
   toExpr = toExpr . map (\(k, a) -> toExpr k :==: toExpr a) . M.toList
   fromExpr expr = do
      eqs <- fromExpr expr
      xs  <- forM eqs $ \(a :==: b) ->
                liftM2 (,) (fromExpr a) (fromExpr b)
      return (M.fromList xs)

instance IsExpr Key where
   toExpr (Key s) = Var s
   fromExpr       = liftM Key . getVariable

clipboard :: ExprVar (M.Map Key Expr)
clipboard = exprVar "clipboard" M.empty
   
addToClipboard :: String -> Expr -> ContextMonad ()
addToClipboard s a = modifyExprVar clipboard (M.insert (Key s) a)

addListToClipboard :: [String] -> [Expr] -> ContextMonad ()
addListToClipboard = zipWithM_ addToClipboard

lookupClipboard :: String -> ContextMonad Expr
lookupClipboard s = do 
   m <- readExprVar clipboard
   maybeCM (M.lookup (Key s) m)
   
lookupListClipboard :: [String] -> ContextMonad [Expr]
lookupListClipboard = mapM lookupClipboard