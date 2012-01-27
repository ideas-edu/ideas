{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
   ( buggyBalanceRules, buggyBalanceExprRules, buggyPriority
   ) where

import Common.Library
import Control.Monad
import Data.Ratio
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.BalanceUtils
import Domain.Math.Polynomial.Views

buggyBalanceRules :: [Rule (Equation Expr)]
buggyBalanceRules =
   [ rule1234, rule201, rule2111, rule2112, rule2113, rule2114
   , rule2121, rule2122, rule2131, rule2132, rule2133, rule2134
   , rule2135, rule2136, rule2137, rule2138, rule2141, rule2142
   , rule221, rule222, rule2232, rule2233
   , rule311, rule322, rule2231, rule227, rule321, rule323
   ]

buggyBalanceExprRules :: [Rule Expr]
buggyBalanceExprRules =
   [ rule121, rule122, rule1231, rule1232, rule1311, rule1312
   , rule1314, rule1321, rule1322, rule133, rule134, rule135
   , rule136, rule137
   ]

buggyPriority :: [Id]
buggyPriority = 
   [ getId rule1312, getId rule121, getId rule221, getId rule222
   , getId rule2232, getId rule2233, getId rule227, getId rule323
   ]

-------------------------------------------------------------------
-- 1.2 Fout bij vermenigvuldigen

-- (a*b)/c  ->  a/(b*c)
rule121 :: Rule Expr
rule121 = describe "1.2.1: fout bij vermenigvuldigen" $
   buggyBalanceExprRule "multiply1" f
 where
   f (expr :/: c) = do
      (a, b) <- match timesView expr
      return $ a/(b*c)
   f _ = Nothing

-- a*(bx+c)  ->  x/(ab) + ac
rule122 :: Rule Expr
rule122 = describe "1.2.2: fout bij vermenigvuldigen" $
   buggyBalanceExprRule "multiply2" f
 where
   f (a :*: expr) = do
      ((b, x), c) <- match (plusView >>> first timesView) expr
      return $ x/(a*b) + a*c
   f _ = Nothing

-- a(b-cx)  -> ab+acx
rule1231 :: Rule Expr
rule1231 = describe "1.2.3.1: fout bij vermenigvuldigen; min raakt kwijt" $
   buggyBalanceExprRule "multiply3" f
 where
   f (a :*: expr) = do
      (b, (c, x)) <- match (minusView >>> second timesView) expr
      return $ a*b+a*c*x
   f _ = Nothing

-- -a*(x-b)  -> -ax-ab
rule1232 :: Rule Expr
rule1232 = describe "1.2.3.2: fout bij vermenigvuldigen; min te veel" $
   buggyBalanceExprRule "multiply4" f
 where
   f expr = do
      (a, (x, b)) <- match (timesView >>> negView *** minusView) expr
      return $ -a*x-a*b

-- -ax=b  ->  x=b/a
rule1234 :: Rule (Equation Expr)
rule1234 = describe "1.2.3.4: fout bij vermenigvuldigen; delen door negatief getal" $
   buggyBalanceRule "multiply5" f
 where
   f (expr :==: b) = do
      (a, x) <- match (timesView >>> first negView) expr
      return $ x :==: b/a

-------------------------------------------------------------------
-- 1.3 Fout bij haakjes wegwerken

-- a(x-b)  ->  ax-b    (verruimt naar +)
rule1311 :: Rule Expr
rule1311 = describe "1.3.1.1: fout bij haakjes wegwerken; haakjes staan er niet voor niets" $
   buggyBalanceExprRule "par1" f
 where
   f expr = do
      (a, (x, b)) <- match (timesView >>> second plusView) expr
      return $ a*x+b

-- 1/a*(x-b)  -> 1/a*x-b   (specialized version of par1)
rule1312 :: Rule Expr
rule1312 = describe "1.3.1.2: fout bij haakjes wegwerken; haakjes staan er niet voor niets" $
   buggyBalanceExprRule "par2" f
 where
   f (e1 :*: e2) = do
      (n, a) <- match (divView >>> first integerView) e1
      guard (n==1)
      (x, b) <- match plusView e2
      return $ 1/a*x+b
   f _ = Nothing

-- a(b-cx)  -> ab-cx
{- zie par1
rule1313 :: Rule (Equation Expr)
rule1313 = describe "1.3.1.3: fout bij haakjes wegwerken; haakjes staan e
r niet voor niets" $

   buggyBalanceExprRule "par3") f
 where
   f (a :*: expr) = do
      (b, (c, x)) <- match (minusView >>> second timesView) expr
      return $ a*b-c*x
   f _ = Nothing -}

-- -(a+b)  ->  -a+b
rule1314 :: Rule Expr
rule1314 = describe "1.3.1.4: fout bij haakjes wegwerken met unaire min; haakjes staan er niet voor niets" $
   buggyBalanceExprRule "par11" f
 where
   f expr = do
      (a, b) <- match (negView >>> plusView) expr
      return $ -a+b

-- a(bx+c)  ->  ax+ac
rule1321 :: Rule Expr
rule1321 = describe "1.3.2.1: fout bij haakjes wegwerken; haakjes goed uitwerken" $
   buggyBalanceExprRule "par4" f
 where
   f (a :*: expr) = do
      ((_, x), c) <- match (plusView >>> first timesView) expr
      return $ a*x+a*c
   f _ = Nothing

-- a(b-cx)  -> ab-ax
rule1322 :: Rule Expr
rule1322 = describe "1.3.2.2: fout bij haakjes wegwerken; haakjes goed uitwerken" $
   buggyBalanceExprRule "par5" f
 where
   f (a :*: expr) = do
      (b, (_, x)) <- match (minusView >>> second timesView) expr
      return $ a*b-a*x
   f _ = Nothing

-- a(bx+c)  -> bx+ac
rule133 :: Rule Expr
rule133 = describe "1.3.3: fout bij haakjes wegwerken; haakjes goed uitwerken" $
   buggyBalanceExprRule "par6" f
 where
   f (a :*: expr) = do
      ((b, x), c) <- match (plusView >>> first timesView) expr
      return $ b*x+a*c
   f _ = Nothing

-- a-(b+c)  -> a-b+c
rule134 :: Rule Expr
rule134 = describe "1.3.4: fout bij haakjes wegwerken; haakjes goed uitwerken" $
   buggyBalanceExprRule "par7" f
 where
   f expr = do
      (a, (b, c)) <- match (minusView >>> second plusView) expr
      return $ a-b+c

-- a*(b-c)-d  ->  ab-ac-ad
rule135 :: Rule Expr
rule135 = describe "1.3.5: fout bij haakjes wegwerken; kijk goed waar de haakjes staan" $
   buggyBalanceExprRule "par8" f
 where
   f expr = do
      ((a, (b, c)), d) <- match (minusView >>> first (timesView >>> second minusView)) expr
      return $ a*b-a*c-a*d

--  a(bx+c)  ->  (a+b)x+ac
rule136 :: Rule Expr
rule136 = describe "1.3.6: fout bij haakjes wegwerken; haakjes goed uitwerken" $
   buggyBalanceExprRule "par9" f
 where
   f (a :*: expr) = do
      ((b, x), c) <- match (plusView >>> first timesView) expr
      return $ (a+b)*x+a*c
   f _ = Nothing

-- a+b(x-c)  -> (a+b)(x-c)
rule137 :: Rule Expr
rule137 = describe "1.3.7: fout bij haakjes wegwerken; denk aan 'voorrangsregels'" $
   buggyBalanceExprRule "par10" f
 where
   f (a :+: expr) = do
      (b, (x, c)) <- match (timesView >>> second plusView) expr
      return $ (a+b)*(x+c)
   f _ = Nothing

-------------------------------------------------------------------
-- 2.0 Links en rechts hetzelfde doen, of verwisselen

-- a=b-c  ->  c-b=a
rule201 :: Rule (Equation Expr)
rule201 = describe "2.0.1: Links en rechts alleen maar verwisseld?" $
   buggyBalanceRule "flip1" f
 where
   f (a :==: rhs) = do
      (b, c) <- match minusView rhs
      return $ c-b :==: a

-------------------------------------------------------------------
-- 2.1 Links en rechts hetzelfde optellen/aftrekken

{- 

   schema addbal regels: (telkens paren met positief/negatief argument)
   1+2   constante naar rechts
   3+4   variabele naar links
   7+8   variabele naar rechts
   9+10  constante naar links
   ---
   5/6    constante links  weggehaald, maar rechts onveranderd gelaten
   11/12  constante rechts weggehaald, maar links  onveranderd gelaten
   13/14  variabele links  weggehaald, maar rechts onveranderd gelaten
   15/16  variabele rechts weggehaald, maar links  onveranderd gelaten
-}

-- ax+b=[cx]+d  -> ax=[cx]+d+b
rule2111 :: Rule (Equation Expr)
rule2111 = describe "2.1.1.1: Links en rechts hetzelfde optellen; links +b en rechts -b" $
   buggyBalanceRuleArg "addbal1" f
 where
   f (lhs :==: rhs) = do
      (ax, b) <- matchPlusCon lhs
      guard (b>0)
      termRef := fromRational b
      return (ax :==: rhs+fromRational b)

-- ax-b=[cx]+d  -> ax=[cx+d-b
rule2112 :: Rule (Equation Expr)
rule2112 = describe "2.1.1.2: Links en rechts hetzelfde optellen; links -b en rechts +b" $
   buggyBalanceRuleArg "addbal2" f
 where
   f (lhs :==: rhs) = do
      (ax, b) <- matchPlusCon lhs
      guard (b<0)
      termRef := fromRational (abs b)
      return (ax :==: rhs+fromRational b)

-- a=cx+d  -> a+d=cx
rule2113 :: Rule (Equation Expr)
rule2113 = describe "2.1.1.3: Je trekt er rechts {?} vanaf, maar links tel je {?} erbij op." $
   buggyBalanceRuleArg "addbal9" f
 where
   f (lhs :==: rhs) = do
      (cx, d) <- matchPlusCon rhs
      guard (d>0)
      termRef := fromRational d
      return (lhs+fromRational d :==: cx)

-- a=cx-d  -> a-d=cx
rule2114 :: Rule (Equation Expr)
rule2114 = describe "2.1.1.4: Je telt er rechts {?} bij op, maar links trek je {?} er vanaf." $
   buggyBalanceRuleArg "addbal10" f
 where
   f (lhs :==: rhs) = do
      (cx, d) <- matchPlusCon rhs
      guard (d<0)
      termRef := fromRational (abs d)
      return (lhs+fromRational d :==: cx)

-- ax[+b]=cx+d  ->  (a+c)x[+b]=d
rule2121 :: Rule (Equation Expr)
rule2121 = describe "2.1.2.1: Links en rechts hetzelfde optellen; links +cx en rechts -cx" $
   buggyBalanceRuleArg "addbal3" f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLin lhs
      (y, c, d) <- matchLin rhs
      guard (c>0 && x==y)
      termRef := fromRational c*x
      return (fromRational (a+c)*x+fromRational b :==: fromRational d)

-- ax[+b]=-cx+d  -> (a-c)x[+b]=d
rule2122 :: Rule (Equation Expr)
rule2122 = describe "2.1.2.2: Links en rechts hetzelfde optellen; links -cx en rechts +cx" $
   buggyBalanceRuleArg "addbal4" f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLin lhs
      (y, c, d) <- matchLin rhs
      guard (c<0 && x==y)
      termRef := fromRational (abs c)*x
      return (fromRational (a+c)*x+fromRational b :==: fromRational d)

-- ax+b=cx+d  ->  b=(a+c)*x+d
rule2141 :: Rule (Equation Expr)
rule2141 = describe "2.1.4.1: Links en rechts hetzelfde optellen; links -ax en rechts +ax" $
   buggyBalanceRuleArg "addbal7" f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLin lhs
      (y, c, d) <- matchLin rhs
      guard (a>0 && x==y)
      termRef := fromRational a*x
      return (fromRational b :==: fromRational (a+c)*x+fromRational d)

-- -ax+b=cx+d  ->  b=(-a+c)*x+d
rule2142 :: Rule (Equation Expr)
rule2142 = describe "2.1.4.2: Links en rechts hetzelfde optellen; links -cx en rechts +cx" $
   buggyBalanceRuleArg "addbal8" f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLin lhs
      (y, c, d) <- matchLin rhs
      guard (a<0 && x==y)
      termRef := fromRational (abs a)*x
      return (fromRational b :==: fromRational (a+c)*x+fromRational d)

-- ax+b=e  -> ax=e
rule2131 :: Rule (Equation Expr)
rule2131 = describe "2.1.3.1: Links en rechts hetzelfde optellen; links -b rechts niet(s)" $
   buggyBalanceRuleArg "addbal5" f
 where
   f (lhs :==: rhs) = do
      (ax, b) <- matchPlusCon lhs
      guard (b > 0)
      termRef := fromRational b
      return (ax :==: rhs)

-- ax-b=e  -> ax=e
rule2132 :: Rule (Equation Expr)
rule2132 = describe "2.1.3.2: Links en rechts hetzelfde optellen; links +b en rechts niet(s)" $
   buggyBalanceRuleArg "addbal6" f
 where
   f (lhs :==: rhs) = do
      (ax, b) <- matchPlusCon lhs
      guard (b < 0)
      termRef := fromRational (abs b)
      return (ax :==: rhs)

-- e=ax+b  -> e=ax
rule2133 :: Rule (Equation Expr)
rule2133 = describe "2.1.3.3: Links en rechts hetzelfde optellen; rechts -b links niet(s)" $
   buggyBalanceRuleArg "addbal11" f
 where
   f (lhs :==: rhs) = do
      (ax, b) <- matchPlusCon rhs
      guard (b > 0)
      termRef := fromRational b
      return (lhs :==: ax)

-- e=ax-b  -> e=ax
rule2134 :: Rule (Equation Expr)
rule2134 = describe "2.1.3.4: Links en rechts hetzelfde optellen; rechts +b en links niet(s)" $
   buggyBalanceRuleArg "addbal12" f
 where
   f (lhs :==: rhs) = do
      (ax, b) <- matchPlusCon rhs
      guard (b < 0)
      termRef := fromRational (abs b)
      return (lhs :==: ax)

-- ax+b=e  -> b=e
rule2135 :: Rule (Equation Expr)
rule2135 = describe "2.1.3.5: Links en rechts hetzelfde optellen; links -ax rechts niet(s)" $
   buggyBalanceRuleArg "addbal13" f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLin lhs
      guard (a > 0)
      termRef := fromRational a*x
      return (fromRational b :==: rhs)

-- -ax+b=e  -> b=e
rule2136 :: Rule (Equation Expr)
rule2136 = describe "2.1.3.6: Links en rechts hetzelfde optellen; links +ax en rechts niet(s)" $
   buggyBalanceRuleArg "addbal14" f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLin lhs
      guard (a < 0)
      termRef := fromRational (abs a)*x
      return (fromRational b :==: rhs)

-- e=ax+b  -> e=b
rule2137 :: Rule (Equation Expr)
rule2137 = describe "2.1.3.7: Links en rechts hetzelfde optellen; rechts -ax links niet(s)" $
   buggyBalanceRuleArg "addbal15" f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLin rhs
      guard (a > 0)
      termRef := fromRational a*x
      return (lhs :==: fromRational b)

-- e=-ax+b  -> e=b
rule2138 :: Rule (Equation Expr)
rule2138 = describe "2.1.3.8: Links en rechts hetzelfde optellen; rechts +ax en links niet(s)" $
   buggyBalanceRuleArg "addbal16" f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLin rhs
      guard (a < 0)
      termRef := fromRational (abs a)*x
      return (lhs :==: fromRational b)
      
-------------------------------------------------------------------
-- 2.2 Links en rechts hetzelfde vermenigvuldigen/delen

-- ax=c  -> x=a/c
rule221 :: Rule (Equation Expr)
rule221 = describe "2.2.1: Links en rechts hetzelfde vermenigvuldigen; verkeerd om gedeeld" $
   buggyBalanceRule "mulbal1" f
 where
   f (expr :==: c) = do
      (a, x) <- match timesView expr
      return $ x :==: a/c

-- 1/*a+b=2/c*x+d  -> x+ba  -> 2x+cd
rule222 :: Rule (Equation Expr)
rule222 = describe "2.2.2: Links en rechts hetzelfde vermenigvuldigen; links *a; rechts *b" $
   buggyBalanceRuleArg "mulbal2" f
 where
   f (lhs :==: rhs) = do
      (x, ra, b) <- matchLin lhs
      (y, rc, d) <- matchLin rhs
      let a = denom ra
          c = denom rc
          denom = fromInteger . denominator
          num   = fromInteger . numerator
      guard (a /= c && (a /= 1 || c /= 1))
      factor1Ref := a
      factor2Ref := c
      return (num ra*x+fromRational b*a :==: num rc*y+c*fromRational d)

-- ax-b=cx+d  -> pax-pb=cx+d
rule2231 :: Rule (Equation Expr)
rule2231 = describe "2.2.3.1: Links en rechts hetzelfde vermenigvuldigen; links *p, rechts niet (of andersom)" $
   buggyBalanceRecognizer "mulbal3" p
 where -- currently, symmetric
   p (a1 :==: a2) (b1 :==: b2) = do
      dl <- diffTimes a1 b1
      dr <- diffTimes a2 b2
      guard ((dl==1) /=  (dr==1)) -- xor; only one of dl/dr equals 1
      factorRef := if dr/=1 then dr else dl

-- (x+a)/b=c  -> x+a=c
rule2232 :: Rule (Equation Expr)
rule2232 = describe "2.2.3.2: Links en rechts hetzelfde vermenigvuldigen; links /p, rechts niet" $
   buggyBalanceRuleArg "mulbal4" f
 where
   f (expr :==: c) = do
      (a, b) <- matchM divView expr
      factorRef := b
      return (a :==: c)

-- a+b=c  -> -a-b=c
rule2233 :: Rule (Equation Expr)
rule2233 = describe "2.2.3.3: Links en rechts hetzelfde vermenigvuldigen; links en rechts *-1" $
   buggyBalanceRule "mulbal5" f
 where
   f (expr :==: c) = do
      (a, b) <- match plusView expr
      return $ -a-b :==: c

-- pa+pb=c -> a+b=c
rule227 :: Rule (Equation Expr)
rule227 = describe "2.2.7: Links en rechts hetzelfde vermenigvuldigen; een kant door p delen, andere kant niets" $
   buggyBalanceRecognizer "mulbal6" p
 where -- currently, symmetric
   p (a1 :==: a2) (b1 :==: b2) = do
      dl <- diffTimes a1 b1
      dr <- diffTimes a2 b2
      rl <- matchM rationalView dl
      rr <- matchM rationalView dr
      guard ( rl == 1 && rr /= 1 && numerator rr == 1 ||
              rl /= 1 && rr == 1 && numerator rl == 1 )
      factorRef := fromIntegral (denominator (if rr /= 1 then rr else rl))

-------------------------------------------------------------------
-- 3.1 Doe je wat je wilt doen?

-- ax-b=cx-d  -> (c-a)x-b=-d
rule311 :: Rule (Equation Expr)
rule311 = describe "3.1.1: Doe je wat je wilt doen?" $
   buggyBalanceRule "misc1" f
 where
   f (lhs :==: rhs) = do
      (x, a, b) <- matchLin lhs
      (y, c, d) <- matchLin rhs
      guard (x==y)
      return (fromRational (c-a)*x+fromRational b :==: fromRational d)

-- ax-b=cd+d  -> pax-b=pcx+pd
rule321 :: Rule (Equation Expr)
rule321 = describe "3.2.1: Doe je wat je wilt doen? vermenigvuldig de hele linkerkant met p" $
   buggyBalanceRecognizer "misc2" p
 where -- currently, not symmetric
   p (a1 :==: a2) (b1 :==: b2) = do
      d <- diffTimes a2 b2
      let as = from simpleSumView a1
      guard (d `notElem` [1, -1] && length as > 1)
      guard $ flip any (take (length as) [0..]) $ \i ->
         let (xs,y:ys) = splitAt i as
             aps = to sumView $ map (d*) xs ++ [y] ++ map (d*) ys
         in viewEquivalent (polyViewWith rationalView) aps b1
      factorRef := d

-- a-b=c  -> -a-b=-c
rule322 :: Rule (Equation Expr)
rule322 = describe "3.2.2: Doe je wat je wilt doen? neem het tegengestelde van de hele linkerkant" $
   buggyBalanceRule "misc3" f
 where
   f (expr :==: c) = do
      (a, b) <- match minusView expr
      return $ -a-b :==: -c

-- pax+pb=pc  ->  ax+pb=c
rule323 :: Rule (Equation Expr)
rule323 = describe "3.2.3: Doe je wat je wilt doen? Deel de hele linkerkant door p" $
   buggyBalanceRecognizer "misc4" p
   -- REFACTOR: code copied from rule misc2
 where -- currently, not symmetric
   p (a1 :==: a2) (b1 :==: b2) = do
      d  <- diffTimes a2 b2
      dr <- matchM rationalView d
      let as = from simpleSumView a1
      guard (dr `notElem` [0, 1, -1] && numerator dr == 1 && length as > 1)
      guard $ flip any (take (length as) [0..]) $ \i ->
         let (xs,y:ys) = splitAt i as
             aps = to sumView $ map (d*) xs ++ [y] ++ map (d*) ys
         in viewEquivalent (polyViewWith rationalView) aps b1
      factorRef := fromRational (1/dr)