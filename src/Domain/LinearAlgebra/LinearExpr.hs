-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.LinearExpr 
   ( IsLinear(..), var, isVar, isConstant, renameVariables
   , splitLinearExpr, evalLinearExpr
   ) where

import Control.Monad
import Data.List
import Common.Uniplate
import Data.Maybe 
import Common.Uniplate
import Common.View hiding (simplify)
import GHC.Real
import Domain.Math.SExpr
import Domain.Math.Constrained
import Domain.Math.Expr
import Domain.Math.Symbolic
import Domain.Math.Views (sumView)
import qualified Data.Map as M

data LinearMap a = LM { lmMap :: M.Map String a, lmConstant :: a }

instance Functor LinearMap where
   fmap f (LM m c) = LM (M.map f m) (f c)

linearView :: View Expr (LinearMap Expr)
linearView = makeView f g
 where 
   -- compositional (sumView would be a more restrictive alternative)
   f = fmap (fmap (fromConstrained . simplify)) . foldExpr alg
   alg = (liftM2 plus, liftJ2 times, liftM2 minus, liftM neg, nat, liftJ2 dv, liftJ sq, var, \f xs -> sequence xs >>= sym f)
    where
      nat n = return $ LM M.empty (fromInteger n)
      var s = return $ LM (M.singleton s 1) 0
      
      plus (LM m1 c1) (LM m2 c2) = 
         LM (M.unionWith (+) m1 m2) (c1+c2)
      neg (LM m c) = 
         LM (M.map negate m) (negate c)
      minus p1 p2 = 
         plus p1 (neg p2)
      
      times lm1@(LM m1 c1) lm2@(LM m2 c2) 
         | M.null m1 = return $ fmap (c1*) lm2
         | M.null m2 = return $ fmap (*c2) lm1
         | otherwise = Nothing
      dv lm (LM m2 c2) = do
         guard (M.null m2 && c2 /= 0)
         return $ fmap (/c2) lm
      
      sq (LM m c) = do
         guard (M.null m)
         return $ LM M.empty (sqrt c)
      sym f ps = do
         guard (all (M.null . lmMap) ps)
         return $ LM M.empty (function f (map lmConstant ps))
   
   g (LM m c) = build sumView (concatMap make (M.toList m) ++ [c | c /= 0])
   make (s, e)
      | e == 0    = []
      | e == 1    = [variable s]
      | e == -1   = [negate (variable s)]
      | otherwise = [e*variable s]

liftJ :: Monad m => (a -> m b) -> m a -> m b
liftJ f ma = ma >>= \a -> f a

liftJ2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJ2 f ma mb = ma >>= \a -> mb >>= \b -> f a b

class (Fractional a, Symbolic a) => IsLinear a where
   isLinear :: a -> Bool
   isVariable :: a -> Maybe String
   getVars  :: a -> [String]
   getConstant     :: a -> a
   coefficientOf   :: String -> a -> a

instance IsLinear Expr where

   isLinear expr = belongsTo expr linearView
      
      {- 
      case expr of
         a :+: b  -> isLinear a && isLinear b
         a :*: b  -> (isConstant a && isLinear b) || (isConstant b && isLinear a)
         a :-: b  -> isLinear a && isLinear b
         Negate a -> isLinear a
         a :/: b  -> (isConstant a && isLinear b) || (isConstant b && isLinear a)
         Var _    -> True
         _        -> isConstant expr -}
         
   isVariable expr =
      case expr of 
         Var s -> Just s
         _     -> Nothing
   
   getVars = collectVars
   
   getConstant expr = 
      case match linearView expr of
         Just (LM _ c) -> c
         _             -> 0
   
      {-
      case expr of
         a :+: b  -> getConstant a + getConstant b
         a :-: b  -> getConstant a - getConstant b
         Negate a -> negate (getConstant a)
         a :*: b  -> getConstant a * getConstant b
         a :/: b  -> getConstant a / b
         _ | isConstant expr -> expr
           | otherwise       -> 0 -}

   coefficientOf s expr = 
      case match linearView expr of
         Just (LM m _) -> M.findWithDefault 0 s m
         _             -> 0
   
   {- 
      case expr of
         a :+: b  -> coefficientOf s a + coefficientOf s b
         a :-: b  -> coefficientOf s a - coefficientOf s b
         Negate a -> negate (coefficientOf s a)
         a :*: b | isConstant a -> a * coefficientOf s b
                 | isConstant b -> b * coefficientOf s a
         a :/: b | isConstant b -> coefficientOf s a / b
         Var t | s==t -> 1
         _            -> 0 -}

instance IsLinear SExpr where
   isLinear = isLinear . toExpr
   isVariable = isVariable . toExpr
   getVars    = getVars . toExpr
   getConstant = simplifyExpr . getConstant . toExpr
   coefficientOf s = simplifyExpr . coefficientOf s . toExpr 

splitLinearExpr :: IsLinear a => (String -> Bool) -> a -> (a, a)
splitLinearExpr f a = (make (getConstant a) xs, make 0 ys)
 where
   (xs, ys) = partition f (getVars a)
   make = foldr (\v r -> coefficientOf v a * var v + r)

evalLinearExpr :: (IsLinear a, Uniplate a) => (String -> a) -> a -> a
evalLinearExpr f a =
   case isVariable a of
      Just s  -> f s
      Nothing -> g $ map (evalLinearExpr f) cs
 where
   (cs, g) = uniplate a

renameVariables :: (IsLinear a, Uniplate a) => (String -> String) -> a -> a
renameVariables f a = 
   case isVariable a of
      Just s  -> variable (f s)
      Nothing -> g $ map (renameVariables f) cs
 where
   (cs, g) = uniplate a

isConstant :: IsLinear a => a -> Bool
isConstant = null . getVars

var :: IsLinear a => String -> a
var = variable

isVar :: IsLinear a => a -> Maybe String
isVar = isVariable