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
import Data.Maybe
import Data.Ratio
import Domain.Math.Expr
import Domain.Math.Data.Relation
import Domain.Math.Polynomial.BalanceUtils
import Domain.Math.Numeric.Views

buggyBalanceRules :: [Rule (Equation Expr)]
buggyBalanceRules = 
   [ rule121, rule122, rule1231, rule1232, rule1234
   , rule1311, rule1312, rule1321, rule1322
   , rule133, rule134, rule135, rule136, rule137
   , rule201
   , rule2111, rule2112, rule2121, rule2122, rule2131, rule2132
   , rule221, rule222, rule2231, rule2232, rule2233, rule227
   , rule311, rule321, rule322, rule323
   ]

buggyPriority :: [Id]
buggyPriority = map getId 
   [rule1312, rule121, rule221, rule222, rule2232, rule2233, rule227]

toEq :: MonadPlus m => (Expr -> m Expr) -> Equation Expr -> m (Equation Expr)
toEq f (lhs :==: rhs) = 
   liftM (:==: rhs) (rec lhs) `mplus` liftM (lhs :==:) (rec rhs)
 where -- to do: deal with associativity
   rec = msum .  map (\(a,h) -> liftM h (f a)) . contexts

bugbal :: Id
bugbal = linbal # "buggy"

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
   
-------------------------------------------------------------------
-- 2.0 Links en rechts hetzelfde doen, of verwisselen

-- a=b-c  ->  c-b=a
rule201 :: Rule (Equation Expr)
rule201 = describe "2.0.1: Links en rechts alleen maar verwisseld?" $ 
   buggyRule $ makeSimpleRule (bugbal, "flip1") f
 where
   f (a :==: rhs) = do
      (b, c) <- match minusView rhs
      return $ c-b :==: a

-------------------------------------------------------------------
-- 2.1 Links en rechts hetzelfde optellen/aftrekken

-- ax+b=[cx]+d  -> ax=[cx]+d+b
rule2111 :: Rule (Equation Expr)
rule2111 = describe "2.1.1.1: Links en rechts hetzelfde optellen; links +b en rechts -b" $ 
   buggyRule $ makeSimpleRule (bugbal, "addbal1") f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLinG lhs
      guard (b>0)
      return $ a*x :==: rhs+b

-- ax-b=[cx]+d  -> ax=[cx+d-b
rule2112 :: Rule (Equation Expr)
rule2112 = describe "2.1.1.2: Links en rechts hetzelfde optellen; links -b en rechts +b" $ 
   buggyRule $ makeSimpleRule (bugbal, "addbal2") f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLinG lhs
      guard (b<0)
      return $ a*x :==: rhs+b
   
-- ax[+b]=cx+d  ->  (a+c)x[+b]=d
rule2121 :: Rule (Equation Expr)
rule2121 = describe "2.1.2.1: Links en rechts hetzelfde optellen; links +cd en rechts -cx" $ 
   buggyRule $ makeSimpleRule (bugbal, "addbal3") f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLinG lhs
      (y, c, d) <- matchLinG rhs
      guard (c>0 && x==y) 
      return $ (a+c)*x+b :==: d
   
-- ax[+b]=-cx+d  -> (a-c)x[+b]=d
rule2122 :: Rule (Equation Expr)
rule2122 = describe "2.1.2.2: Links en rechts hetzelfde optellen; links -cx en rechts +cx" $ 
   buggyRule $ makeSimpleRule (bugbal, "addbal4") f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLinG lhs
      (y, c, d) <- matchLinG rhs
      guard (c<0 && x==y) 
      return $ (a+c)*x+b :==: d
   
-- ax+b=[cx]+d  -> ax=[cx]+d
rule2131 :: Rule (Equation Expr)
rule2131 = describe "2.1.3.1: Links en rechts hetzelfde optellen; links -b rechts niet(s)" $ 
   buggyRule $ makeSimpleRule (bugbal, "addbal5") f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLinG lhs
      guard ((b::Expr) > 0)
      return $ a*x :==: rhs
   
-- ax-b=[cx]+d  -> ax=[cx]+d
rule2132 :: Rule (Equation Expr)
rule2132 = describe "2.1.3.2: Links en rechts hetzelfde optellen; links+b en rechts niet(s)" $ 
   buggyRule $ makeSimpleRule (bugbal, "addbal6") f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLinG lhs
      guard ((b::Expr) < 0)
      return $ a*x :==: rhs
      
-------------------------------------------------------------------
-- 2.2 Links en rechts hetzelfde vermenigvuldigen/delen

-- ax=c  -> x=a/c
rule221 :: Rule (Equation Expr)
rule221 = describe "2.2.1: Links en rechts hetzelfde vermenigvuldigen; verkeerd om gedeeld" $ 
   buggyRule $ makeSimpleRule (bugbal, "mulbal1") f
 where
   f (expr :==: c) = do
      (a, x) <- match timesView expr
      return $ x :==: a/c
      
-- 1/*a+b=2/c*x+d  -> x+ba  -> 2x+cd
rule222 :: Rule (Equation Expr)
rule222 = describe "2.2.2: Links en rechts hetzelfde vermenigvuldigen; links *a; rechts *b" $ 
   buggyRule $ makeSimpleRule (bugbal, "mulbal2") f
 where
   f (lhs :==: rhs) = do
      (x, ra, b) <- matchLinG lhs
      (y, rc, d) <- matchLinG rhs
      let a = denom ra
          c = denom rc
          denom = fromInteger . denominator
          num   = fromInteger . numerator
      guard (a /= 1 || b /= 1)
      return $ num ra*x+b*a :==: num rc*y+c*d
      
-- ax-b=cx+d  -> pax-pb=cx+d
rule2231 :: Rule (Equation Expr)
rule2231 = describe "2.2.3.1: Links en rechts hetzelfde vermenigvuldigen; links *p, rechts niet" $ 
   buggyRule $ makeRule (bugbal, "mulbal3") $ useRecognizer p $ 
   makeTrans $ const Nothing
 where
   p (a1 :==: a2) (b1 :==: b2) = fromMaybe False $ do
      dl <- diffTimes a1 b1
      dr <- diffTimes a2 b2
      return $ (dl == 1 && dr /= 1) || (dl /= 1 && dr == 1)
      
-- (x+a)/b=c  -> x+a=c
rule2232 :: Rule (Equation Expr)
rule2232 = describe "2.2.3.2: Links en rechts hetzelfde vermenigvuldigen; links /p, rechts niet" $ 
   buggyRule $ makeSimpleRule (bugbal, "mulbal4") f
 where
   f (expr :==: c) = do
      (a, _b) <- match divView expr
      return $ a :==: c
      
-- a+b=c  -> -a-b=c
rule2233 :: Rule (Equation Expr)
rule2233 = describe "2.2.3.3: Links en rechts hetzelfde vermenigvuldigen; links en rechts *-1" $ 
   buggyRule $ makeSimpleRule (bugbal, "mulbal5") f
 where
   f (expr :==: c) = do
      (a, b) <- match plusView expr
      return $ -a-b :==: c
      
-- pa+pb=c -> a+b=c
rule227 :: Rule (Equation Expr)
rule227 = describe "2.2.7: Links en rechts hetzelfde vermenigvuldigen; links en rechts delen door p" $ 
   buggyRule $ makeRule (bugbal, "mulbal6") $ useRecognizer p $ 
   makeTrans $ const Nothing
 where
   p (a1 :==: a2) (b1 :==: b2) = fromMaybe False $ do
      dl <- diffTimes a1 b1
      dr <- diffTimes a2 b2
      rl <- match rationalView dl
      rr <- match rationalView dr
      return $ (rl == 1 && rr /= 1 && numerator rr == 1)
            || (rl /= 1 && rr == 1 && numerator rl == 1)
      
-------------------------------------------------------------------
-- 3.1 Doe je wat je wilt doen?

-- ax-b=cx-d  -> (c-a)x-b=-d
rule311 :: Rule (Equation Expr)
rule311 = describe "3.1.1: Doe je wat je wilt doen?" $ 
   buggyRule $ makeSimpleRule (bugbal, "misc1") f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLinG lhs
      (y, c, d) <- matchLinG rhs
      guard (x==y)
      return ((c-a)*x+b :==: d)

-- ax-b=cd+d  -> pax-b=pcx+pd
rule321 :: Rule (Equation Expr)
rule321 = describe "3.2.1: Doe je wat je wilt doen? vermenigvuldig de hele linkerkant met p" $ 
   buggyRule $ makeSimpleRule (bugbal, "misc2") f
 where
   f _ = Nothing

-- a-b=c  -> -a-b=-c
rule322 :: Rule (Equation Expr)
rule322 = describe "3.2.2: Doe je wat je wilt doen? neem het tegengestelde van de hele linkerkant" $ 
   buggyRule $ makeSimpleRule (bugbal, "misc3") f
 where
   f (expr :==: c) = do
      (a, b) <- match minusView expr
      return $ -a-b :==: -c
   
-- pax+pb=pc  ->  ax+pb=c
rule323 :: Rule (Equation Expr)
rule323 = describe "3.2.3: Doe je wat je wilt doen? Deel de hele linkerkant door p" $ 
   buggyRule $ makeSimpleRule (bugbal, "misc4") f
 where
   f _ = Nothing