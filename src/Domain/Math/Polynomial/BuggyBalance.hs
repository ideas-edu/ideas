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
module Domain.Math.Polynomial.BuggyBalance 
   ( buggyBalanceRules, buggyPriority
   ) where

import Control.Monad
import Common.Library
import Common.Utils.Uniplate
import Domain.Math.Expr
import Domain.Math.Data.Relation
import Domain.Math.Numeric.Views

buggyBalanceRules :: [Rule (Equation Expr)]
buggyBalanceRules = 
   [ rule121, rule122, rule1231, rule1232, rule1234
   , rule1311, rule1312, rule1321, rule1322
   , rule133, rule134, rule135, rule136, rule137
   ]

buggyPriority :: [Id]
buggyPriority = [getId rule1312]

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
      
-------------------------------------------------------------------
-- 1.3 Fout bij haakjes wegwerken

-- a(x-b)  ->  ax-b
rule1311 :: Rule (Equation Expr)
rule1311 = describe "1.3.1.1: fout bij haakjes wegwerken; haakjes staan er niet voor niets" $ 
   buggyRule $ makeSimpleRule (bugbal, "par1") (toEq f)
 where
   f (a :*: expr) = do
      (x, b) <- match minusView expr
      return $ a*x-b
   f _ = Nothing
   
-- 1/a*(x-b)  -> 1/a*x-b   (specialized version of par1)
rule1312 :: Rule (Equation Expr)
rule1312 = describe "1.3.1.2: fout bij haakjes wegwerken; haakjes staan er niet voor niets" $ 
   buggyRule $ makeSimpleRule (bugbal, "par2") (toEq f)
 where
   f (e1 :*: e2) = do
      (n, a) <- match (divView >>> first integerView) e1
      guard (n==1)
      (x, b) <- match minusView e2
      return $ 1/a*x-b
   f _ = Nothing
   
-- a(b-cx)  -> ab-cx
{- zie par1
rule1313 :: Rule (Equation Expr)
rule1313 = describe "1.3.1.3: fout bij haakjes wegwerken; haakjes staan e
r niet voor niets" $ 
   buggyRule $ makeSimpleRule (bugbal, "par3") (toEq f)
 where
   f (a :*: expr) = do
      (b, (c, x)) <- match (minusView >>> second timesView) expr
      return $ a*b-c*x
   f _ = Nothing -}
   
-- a(bx+c)  ->  ax+ac
rule1321 :: Rule (Equation Expr)
rule1321 = describe "1.3.2.1: fout bij haakjes wegwerken; haakjes goed uitwerken" $ 
   buggyRule $ makeSimpleRule (bugbal, "par4") (toEq f)
 where
   f (a :*: expr) = do
      ((_, x), c) <- match (plusView >>> first timesView) expr
      return $ a*x+a*c
   f _ = Nothing
   
-- a(b-cx)  -> ab-ax
rule1322 :: Rule (Equation Expr)
rule1322 = describe "1.3.2.2: fout bij haakjes wegwerken; haakjes goed uitwerken" $ 
   buggyRule $ makeSimpleRule (bugbal, "par5") (toEq f)
 where
   f (a :*: expr) = do
      (b, (_, x)) <- match (minusView >>> second timesView) expr
      return $ a*b-a*x
   f _ = Nothing
   
-- a(bx+c)  -> bx+ac
rule133 :: Rule (Equation Expr)
rule133 = describe "1.3.3: fout bij haakjes wegwerken; haakjes goed uitwerken" $ 
   buggyRule $ makeSimpleRule (bugbal, "par6") (toEq f)
 where
   f (a :*: expr) = do
      ((b, x), c) <- match (plusView >>> first timesView) expr
      return $ b*x+a*c
   f _ = Nothing
   
-- a-(b+c)  -> a-b+c
rule134 :: Rule (Equation Expr)
rule134 = describe "1.3.4: fout bij haakjes wegwerken; haakjes goed uitwerken" $ 
   buggyRule $ makeSimpleRule (bugbal, "par7") (toEq f)
 where
   f expr = do 
      (a, (b, c)) <- match (minusView >>> second plusView) expr
      return $ a-b+c
   
-- a*(b-c)-d  ->  ab-ac-ad
rule135 :: Rule (Equation Expr)
rule135 = describe "1.3.5: fout bij haakjes wegwerken; kijk goed waar de haakjes staan" $ 
   buggyRule $ makeSimpleRule (bugbal, "par8") (toEq f)
 where
   f expr = do 
      ((a, (b, c)), d) <- match (minusView >>> first (timesView >>> second minusView)) expr
      return $ a*b-a*c-a*d
   
--  a(bx+c)  ->  (a+b)x+ac
rule136 :: Rule (Equation Expr)
rule136 = describe "1.3.6: fout bij haakjes wegwerken; haakjes goed uitwerken" $ 
   buggyRule $ makeSimpleRule (bugbal, "par9") (toEq f)
 where
   f (a :*: expr) = do
      ((b, x), c) <- match (plusView >>> first timesView) expr
      return $ (a+b)*x+a*c
   f _ = Nothing
   
-- a+b(x-c)  -> (a+b)(x-c)
rule137 :: Rule (Equation Expr)
rule137 = describe "1.3.7: fout bij haakjes wegwerken; denk aan 'voorrangsregels'" $ 
   buggyRule $ makeSimpleRule (bugbal, "par10") (toEq f)
 where
   f (a :+: expr) = do
      (b, (x, c)) <- match (timesView >>> second minusView) expr
      return $ (a+b)*(x-c)
   f _ = Nothing