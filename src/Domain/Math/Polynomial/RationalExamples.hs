-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Example exercises from the Digital Mathematics Environment (DWO),
-- see: http://www.fi.uu.nl/dwo/gr/frameset.html.
--
-----------------------------------------------------------------------------
module Domain.Math.Polynomial.RationalExamples 
   ( brokenEquations, normBroken, normBroken2, normBrokenCon, deelUit
   ) where

import Prelude hiding ((^))
import Common.Rewriting
import Domain.Math.Data.Relation
import Domain.Math.Expr

----------------------------------------------------------
-- VWO B applets

-- Hoofdstuk 1, gebroken vergelijkingen
brokenEquations :: [[Equation Expr]]
brokenEquations =
   -- Bereken exact de oplossingen
   let x = Var "x" in
   [ [ (2*x^2-10) / (x^2+3) :==: 0
     , (7*x^2-21) / (2*x^2-5) :==: 0
     , (3*x^2-6) / (4*x^2+1) :==: 0
     , (4*x^2-24) / (6*x^2-2) :==: 0
     , x^2 / (x+4) :==: (3*x+4) / (x+4)
     , (x^2+2) / (x-2) :==: (x+8) / (x-2)
     , (x^2+6*x-6)/(x^2-1) :==: (4*x+9)/(x^2-1)
     , (x^2+6)/(x^2-2) :==: (7*x)/(x^2-2)
     ]
   , [ (x^2+6*x)/(x^2-1) :==: (3*x+4)/(x^2-1)
     , (x^2+6)/(x-3) :==: (5*x)/(x-3) 
     , (x^2+4*x)/(x^2-4) :==: (3*x + 6)/(x^2-4)
     , (x^2+2*x-4)/(x-5) :==: (4*x+11)/(x-5)
     , (5*x+2)/(2*x-1) :==: (5*x+2)/(3*x+5)
     , (x^2-9)/(4*x-1) :==: (x^2-9)/(2*x+7)
     , (3*x-2)/(2*x^2) :==: (3*x-2)/(x^2+4)
     , (2*x+1)/(x^2+3*x) :==: (2*x+1)/(5*x+8)
     ]
   , [ (x^2-1)/(2*x+2) :==: (x^2-1)/(x+8)
     , (x^2-4)/(3*x-6) :==: (x^2-4)/(2*x+1)
     , (x^2+5*x)/(2*x^2) :==: (x^2+5*x)/(x^2+4)
     , (x^2-3*x)/(2*x-6) :==: (x^2-3*x)/(4*x+2)
     , x/(x+1) :==: 1 + 3/4
     , (x+2)/(3*x) :==: 1 + 1/3
     , (2*x+3)/(x-1) :==: 3 + 1/2
     , (x-3)/(1-x) :==: 1 + 2/5
     ]
   , [ (x+4)/(x+3) :==: (x+1)/(x+2)
     , (2*x+3)/(x-1) :==: (2*x-1) / (x-2)
     , (3*x+6)/(3*x-1) :==: (x+4)/(x+1)
     , (x+2)/(2*x+5) :==: (x+4)/(2*x-3)
     , (x+5)/(2*x) + 2 :==: 5
     , (3*x+4)/(x+2) - 3 :==: 2
     , (x^2)/(5*x+6) + 4 :==: 5
     , (x^2)/(2*x-3) + 3 :==: 7
     ]
   , [ (x-2)/(x-3) :==: x/2
     , (x+9)/(x-5) :==: 2/x
     , (x+2)/(x+4) :==: 2/(x+1)
     , (-3)/(x-5) :==: (x+3)/(x+1)
     , (x+1)/(x+2) :==: (7*x+1)/(2*x-4)
     , (2*x-7)/(5-x) :==: (x+1)/(3*x-7)
     , (x+1)/(x-1) :==: (3*x-7)/(x-2)
     , (3*x-7)/(x-2) :==: (7-x)/(3*x-3)
     ]
   ]
   
-- Hoofdstuk 4, gebroken vorm herleiden (1 en 1a)
normBroken :: [[Expr]]
normBroken =
   -- Herleid
   let x = Var "x" in
   let y = Var "y" in
   let a = Var "a" in
   let b = Var "b" in
   [ [ 7/(2*x) + 3/(5*x), 3/(2*x) + 2/(3*x), 4/(5*x)-2/(3*x)
     , 2/(7*x) - 1/(4*x), 5/(6*a)+3/(7*a), 3/(8*a)+5/(3*a)
     , 7/(2*a)-2/(3*a),  9/(5*a)-1/(2*a)
     ]
   , [ 1/x+1/y, 2/(3*x)+1/(2*y), 3/(x^2*y) - 5/(2*x*y), 2/(x*y)-7/(5*y)
     , 2/a - 3/b, 4/(3*a)-2/(5*b), 2/(a*b)+4/(3*a), 7/(4*a)+3/(4*b)
     ]
   , [ 3+1/(2*x), 2*x+(3/(5*x)), 5/(2*x)-3, 3-5/(7*x), 5/(3*a)+1
     , 4*a+3/(2*a), 2*a-1/(3*a), 7/(5*a)-2
     ]
   , [ 5/(x+2)+4/(x+3), 3/(x-1)+2/(x+3), 4/(x+5)+2/(x-3), 3/(x-2)+2/(x-3)
     , 4/(x+3)-6/(x+2), 1/(x+5)-3/(x-4), 7/(x-3)-2/(x+1), 6/(x-1)-3/(x-2)
     ]
   , [ (x+1)/(x+2)+(x+2)/(x-3), (x-2)/(x+3)+(x-1)/(x+2), (x+3)/(x-1)+(x+2)/(x-4)
     , (x-4)/(x+5)+(x-2)/(x-3), (x-1)/(x+1)-(x+2)/(x-2), (x+5)/(x+3)-(x+3)/(x+5)
     , (x-1)/(x+2)-(x+4)/(x+1), (x-3)/(x-1)-(x+2)/(x+4)
     ]
   , [ (2*x)/(x-1)+x/(x+2), (3*x)/(x-4)+(5*x)/(x-2)
     , (4*x)/(x+2)-(2*x)/(x+1), x/(x+5)-(4*x)/(x+6)
     ]
   ]

-- Hoofdstuk 4, gebroken vorm herleiden (2 en 2a)
normBroken2 :: [[Expr]]
normBroken2 =
   -- Herleid
   let x = Var "x" in
   let a = Var "a" in
   let p = Var "p" in
   [ [ (x^2+4*x-5)/(x^2+5*x-6), (x^2+2*x-8)/(x^2+10*x+24)
     , (x^2-7*x+12)/(x^2+x-20), (x^2+7*x+12)/(x^2+5*x+6)
     , (a^2-a-2)/(a^2+4*a-12), (a^2-3*a-10)/(a^2-a-20)
     , (a^2-2*a-15)/(a^2-3*a-18), (a^2+a-2)/(a^2+3*a+2)
     ]
   , [ (x^2-16)/(x^2+x-12), (x^2-2*x+1)/(x^2-1), (x^2-9)/(x^2+6*x+9)
     , (x^2-7*x+6)/(x^2-1), (2*p^2+8*p)/(p^2-16), (-(p^2)+5*p)/(p^2-10*p+25)
     , (p^2-4)/(4*p^2+8*p), (p^2-12*p+36)/(p^2-6*p)
     ]
   , [ (x^3+3*x^2+2*x)/(x^2+4*x+4), (x^3+10*x^2+24*x)/(x^2+7*x+6)
     , (x^2+5*x+6)/(x^3-x^2-6*x), (x^2+3*x-4)/(x^3-6*x^2+5*x)
     , (a^3+7*a^2+12*a)/(a^2+6*a+9), (a^3+7*a^2+10*a)/(a^2-a-6)
     , (a^2-9)/(a^3-4*a^2+3*a), (a^2-2*a-15)/(a^3-3*a^2-10*a)
     ]
   ]
   
deelUit :: [[Expr]]
deelUit =
   let x = Var "x" in
   let a = Var "a" in
   let p = Var "p" in
   let t = Var "t" in
   [ -- laatste sommen van gebroken vorm herleiden (2), niveau 5
     [ (-6*a^2-1)/a, -2*p^2+3/(7*p), (7*t^2+4)/(-4*t), (9*x^2+8)/(8*x)
     ]
   , -- sommen (2a)
     [ (-7*a^2-4*a-6)/(-6*a), (3*p^2+6*p-8)/p, (2*t^2-9*t-8)/(-2*t)
     , (x^2+5*x+5)/(2*x), (5*a^3-4*a+2)/(9*a), (5*p^3-7*p^2+9)/(2*p)
     , (-3*t^3+6*t-4)/(3*t), (4*x^3-3*x^2+4)/(7*x)
     ]
   ]
   
-- Vervolg hoofdstuk 4, gebroken vorm herleiden (2 en 2a), vanaf niveau 4
normBrokenCon :: [[Equation Expr]]
normBrokenCon =
   -- Herleid
   let a = Var "a" in
   let p = Var "p" in
   let t = Var "t" in
   let ca = symbol (newSymbol "A") in
   let ct = symbol (newSymbol "T") in
   let cn = symbol (newSymbol "N") in
   [ [ ca :==: (p^2+2*p)/(p^2-4), ca :==: (6*p^2-18*p)/(p^2-9)
     , ca :==: (p^2-1)/(-2*p^2+2*p), ca :==: (p^2-16)/(4*p^2+16*p)
     , ct :==: (t^3-2*t^2)/(t^2-4), ct :==: (t^3+4*t^2)/(t^2-16)
     , ct :==: (t^2-1)/(t^3+t^2), ct :==: (t^2-25)/(t^3-5*t^2)
     ]
   , [ cn :==: (a^4+4*a^2-5)/(a^4-1), cn :==: (a^4+5*a^2+6)/(a^4+4*a^2+3)
     , cn :==: (a^4-5*a^2+6)/(a^4-7*a^2+10), cn :==: (a^4-8*a^2+16)/(a^4-5*a^2+4)
     ]
   ]