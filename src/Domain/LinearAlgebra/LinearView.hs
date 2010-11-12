-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.LinearView
   ( IsLinear(..), LinearMap, renameVariables
   , splitLinearExpr, evalLinearExpr, linearView
   ) where

import Control.Monad
import Data.List
import Common.Rewriting
import Common.Uniplate
import Common.View
import Domain.Math.Expr
import qualified Data.Map as M

data LinearMap a = LM { lmMap :: M.Map String a, lmConstant :: a }

instance Functor LinearMap where
   fmap f (LM m c) = LM (M.map f m) (f c)

linearView :: View Expr (LinearMap Expr)
linearView = makeView f g
 where 
   -- compositional (sumView would be a more restrictive alternative)
   f expr = 
      case expr of
         Nat _    -> return $ LM M.empty expr
         Var s    -> return $ LM (M.singleton s 1) 0
         a :+: b  -> liftM2 plusLM  (f a) (f b)
         a :-: b  -> liftM2 plusLM  (f a) (liftM negateLM (f b))
         Negate a -> liftM negateLM (f a)
         a :*: b  -> join $ liftM2 timesLM (f a) (f b)
         a :/: b  -> join $ liftM2 divLM (f a) (f b)
         Sqrt a   -> join $ liftM sqrtLM (f a)
         Number _ -> return $ LM M.empty expr
         Sym s as -> mapM f as >>= symLM s
       
   g (LM m c) = build sumView (concatMap make (M.toList m) ++ [c | c /= 0])
   make (s, e)
      | e == 0    = []
      | e == 1    = [variable s]
      | e == -1   = [negate (variable s)]
      | otherwise = [e*variable s]

plusLM :: Num a => LinearMap a -> LinearMap a -> LinearMap a
plusLM (LM m1 c1) (LM m2 c2) = LM (M.unionWith (+) m1 m2) (c1+c2)

negateLM :: Num a => LinearMap a -> LinearMap a
negateLM (LM m c) = LM (M.map negate m) (negate c)

timesLM :: Num a => LinearMap a -> LinearMap a -> Maybe (LinearMap a)
timesLM lm1@(LM m1 c1) lm2@(LM m2 c2) 
   | M.null m1 = return $ fmap (c1*) lm2
   | M.null m2 = return $ fmap (*c2) lm1
   | otherwise = Nothing

divLM :: Fractional a => LinearMap a -> LinearMap a -> Maybe (LinearMap a)
divLM lm (LM m2 c2) = do
   guard (M.null m2 && c2 /= 0)
   return $ fmap (/c2) lm

sqrtLM :: Floating a => LinearMap a -> Maybe (LinearMap a)
sqrtLM (LM m c) = do
   guard (M.null m)
   return $ LM M.empty (sqrt c)

symLM :: WithFunctions a => Symbol -> [LinearMap a] -> Maybe (LinearMap a)
symLM f ps = do
   guard (all (M.null . lmMap) ps)
   return $ LM M.empty (function f (map lmConstant ps))

class (Fractional a, Uniplate a, WithVars a) => IsLinear a where
   isLinear      :: a -> Bool
   getConstant   :: a -> a
   coefficientOf :: String -> a -> a

instance IsLinear Expr where
   isLinear        = (`belongsTo` linearView)
   getConstant     = maybe 0 lmConstant . match linearView
   coefficientOf s = maybe 0 (M.findWithDefault 0 s . lmMap) . match linearView

splitLinearExpr :: IsLinear a => (String -> Bool) -> a -> (a, a)
splitLinearExpr f a = (make (getConstant a) xs, make 0 ys)
 where
   (xs, ys) = partition f (vars a)
   make = foldr (\v r -> coefficientOf v a * variable v + r)

evalLinearExpr :: IsLinear a => (String -> a) -> a -> a
evalLinearExpr f a =
   case getVariable a of
      Just s  -> f s
      Nothing -> descend (evalLinearExpr f) a

renameVariables :: IsLinear a => (String -> String) -> a -> a
renameVariables f a = 
   case getVariable a of
      Just s  -> variable (f s)
      Nothing -> descend (renameVariables f) a