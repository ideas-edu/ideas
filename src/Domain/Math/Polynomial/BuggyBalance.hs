{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
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
module Domain.Math.Polynomial.BuggyBalance (buggyBalanceRules) where

import Control.Monad
import Common.Library
import Common.Utils.Uniplate
import Domain.Math.Expr
import Domain.Math.Data.Relation

buggyBalanceRules :: [Rule (Equation Expr)]
buggyBalanceRules = [rule121, rule122, rule1231, rule1232, rule1234]

toEq :: MonadPlus m => (Expr -> m Expr) -> Equation Expr -> m (Equation Expr)
toEq f (lhs :==: rhs) = 
   liftM (:==: rhs) (rec lhs) `mplus` liftM (lhs :==:) (rec rhs)
 where -- to do: deal with associativity
   rec = msum .  map (\(a,h) -> liftM h (f a)) . contexts

minusView :: View Expr (Expr, Expr)
minusView = makeView isMinus (uncurry (:-:))

negView :: View Expr Expr
negView = makeView isNegate Negate

bugbal :: String
bugbal = "algebra.equations.linear.balance.buggy"

-------------------------------------------------------------------
-- 1.2 Fout bij vermenigvuldigen

-- (a*b)/c  ->  a/(b*c)
rule121 :: Rule (Equation Expr)
rule121 = describe "1.2.1: fout bij vermenigvuldigen" $ 
   buggyRule $ makeSimpleRule (bugbal, "multiply1") (toEq f)
 where 
   f (expr :/: c) = do
      (a, b) <- match timesView expr
      return $ a/(b*c)
   f _ = Nothing
   
-- a*(bx+c)  ->  x/(ab) + ac
rule122 :: Rule (Equation Expr)
rule122 = describe "1.2.2: fout bij vermenigvuldigen" $ 
   buggyRule $ makeSimpleRule (bugbal, "multiply2") (toEq f)
 where 
   f (a :*: expr) = do
      ((b, x), c) <- match (plusView >>> first timesView) expr
      return $ x/(a*b) + a*c
   f _ = Nothing

-- a(b-cx)  -> ab+acx
rule1231 :: Rule (Equation Expr)
rule1231 = describe "1.2.3.1: fout bij vermenigvuldigen; min raakt kwijt" $ 
   buggyRule $ makeSimpleRule (bugbal, "multiply3") (toEq f)
 where 
   f (a :*: expr) = do
      (b, (c, x)) <- match (minusView >>> second timesView) expr
      return $ a*b+a*c*x
   f _ = Nothing
   
-- -a*(x-b)  -> -ax-ab
rule1232 :: Rule (Equation Expr)
rule1232 = describe "1.2.3.2: fout bij vermenigvuldigen; min te veel" $ 
   buggyRule $ makeSimpleRule (bugbal, "multiply4") (toEq f)
 where 
   f expr = do
      (a, (x, b)) <- match (timesView >>> negView *** minusView) expr
      return $ -a*x-a*b

-- -ax=b  ->  x=b/a
rule1234 :: Rule (Equation Expr)
rule1234 = describe "1.2.3.4: fout bij vermenigvuldigen; delen door negatief getal" $ 
   buggyRule $ makeSimpleRule (bugbal, "multiply5") f
 where 
   f (expr :==: b) = do
      (a, x) <- match (timesView >>> first negView) expr
      return $ x :==: b/a