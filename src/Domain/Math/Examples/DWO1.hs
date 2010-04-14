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
-- Example exercises from the Digital Mathematics Environment (DWO)
--
-----------------------------------------------------------------------------
module Domain.Math.Examples.DWO1
   ( calculateResults, fillInResult
   , coverUpEquations, linearEquations
   , quadraticEquations, findFactors
   , modulusEquations, sqrtEquations, sqrtSubstEquations, brokenEquations
   , simplerSqrt, simplerSqrt2, simplerSqrt3
   ) where

import Prelude hiding ((^))
import Domain.Math.Data.Relation
import Domain.Math.Expr

calculateResults :: [[Expr]]
calculateResults = [level1, level2, level3]
 where
   level1 = 
      [ -8*(-3)
      , -3-9
      , 55/(-5)
      , -6*9
      , -11- (-3)
      , 6-(-9)
      , -10+3
      , 6+(-5)
      ]
      
   level2 = 
      [ -3-(6*(-3))
      , -12/3 - 3
      , -4*(2+3)
      , 2-6*6
      , -27/(4-(-5))
      , (-24/(-6)) - 3
      , 8-(-77/(-11))
      , 4/(-4+5)
      ]
      
   level3 = 
      [ 4*(3-(6-2))
      , (-16-9)/5 - 3
      , 4- (4-13)/(-3)
      , (3*(-3))-5-4
      , -55/(3*(-5)+4)
      , -4*(-2+ (-4)+7)
      , -8 - (140/4*5)
      , (13-(2-1)) / 3
      ]

fillInResult :: [[Equation Expr]]
fillInResult = [level1, level2, level3]
 where
   level1 = 
      let x = variable "x" in
      [ x-2    :==: 2
      , -4*x   :==: -28
      , -8*x   :==: 72
      , x+4    :==: 09
      , 4+x    :==: 2
      , -10-x  :==: -7
      , x/(-8) :==: -3
      , 11-x   :==: 14
      ]
      
   level2 = 
      let x = variable "x" in
      [ -5-3*x      :==: -23
      , 21/x - 4    :==: 3
      , -3*(x+3)    :==: -27
      , 2-5*x       :==: 47
      , 18/(7-x)    :==: 6
      , -77/x  + 4  :==: -7
      , -7-(x/(-5)) :==: -15
      , -18/(-3+x)  :==: 3
      ]

   level3 = 
      let x = variable "x" in
      [ -5*(5-(3-x))    :==: -20
      , (-20-x)/(-5)-2  :==: 3
      , 4-(x-14)/(-3)   :==: 1
      , 3*x - 3 - 7     :==: 8
      , -42/(-2*x+2)    :==: 7
      , 3*(4+x+2)       :==: 12
      , -6-(-54/(-3*x)) :==: -12
      , 14-(x-3)/4      :==: 3
      ]

coverUpEquations :: [[Equation Expr]]
coverUpEquations = [level1, level2]
 where
   level1 = 
      let x = variable "x" in
      [ 38-7*x       :==: 3
      , sqrt (125/x) :==: 5
      , 4*(12-x) + 7 :==: 35
      , 5*x^2        :==: 80 
      , 5*(5-x)      :==: 35
      , 32/sqrt x    :==: 8
      , (21/x)-8     :==: -1
      , 180/x^2      :==: 5
      , 3*(x-8)^2    :==: 12
      , (8-x)/3 + 7  :==: 9
      ]
   
   level2 = 
      let x = variable "x" in
      [ sqrt (x+9)/2       :==: 3
      , (4*x-18)^2         :==: 4
      , 3*(13-2*x)^2 - 20  :==: 55
      , 5*((x/3) - 8)^2    :==: 20
      , (6/sqrt (x-7))^3   :==: 8
      , 8-(15/sqrt (31-x))           :==: 5
      , sqrt (4*(x^2-21))            :==: 4
      , 3 + (44/sqrt (87 + x))       :==: 7
      , 13-(56 / (21 + (70/(3+x))))  :==: 12
      , 12/(2+(24/(8+(28/(2+9/x))))) :==: 3
      ]  

linearEquations :: [[Equation Expr]]
linearEquations = [level1, level2, level3, level4, level5]
 where
   level1 :: [Equation Expr]
   level1 = 
      let x = variable "x" in
      [ 5*x + 3   :==: 18
      , 11*x - 12 :==: 21
      , 19 - 3*x  :==: -5
      , -12 + 5*x :==: 33
      , 15 - 9*x  :==: 6
      , 4*x + 18  :==: 0
      , 11*x - 12 :==: -34
      , -2*x - 3  :==: -4
      , 6*x - 12  :==: 2
      , -4*x - 13 :==: -11
      ]

   level2 :: [Equation Expr]
   level2 = 
      let x = variable "x" in
      [ 6*x-2    :==: 2*x+14
      , 3+6*x    :==: 3*x+24
      , 5*x+7    :==: 2*x - 10
      , 2*x-8    :==: 18 - x
      , 4*x - 6  :==: 7*x - 14
      , -1 -5*x  :==: 3*x - 20
      , 4*x - 7  :==: -5*x - 24
      , 4*x - 18 :==: 14 + 11*x
      , 17       :==: 4 - 10*x
      , -5*x + 6 :==: 2 - 3*x
      ]

   level3 :: [Equation Expr]
   level3 = 
      let x = variable "x" in
      [ 4*(x-1)          :==: 11*x - 12
      , 4*(x-4)          :==: 5*(2*x+1)
      , 2*(5-3*x)        :==: 6-x
      , 4*x - (x-2)      :==: 12 + 5*(x-1)
      , -3*(x-2)         :==: 3*(x+4) - 7
      , 3*(4*x-1) + 3    :==: 7*x - 14
      , 4*(4*x - 1) - 2  :==: -3*x + 3*(2*x -5)
      , 2*x - (3*x + 5)  :==: 10 + 5*(x-1)
      , -5*(x+1)         :==: 9*(x+4)-5
      , 18 - 2*(4*x + 2) :==: 7*x - 4*(4*x -2)
      ]

   level4 :: [Equation Expr]
   level4 = 
      let x = variable "x" in
      [ (1/2)*x - 4            :==: 2*x + 2+(1/2)
      , (1/4)*x + (1/2)        :==: (5/2)*x + 2
      , (1/4)*x - (3/4)        :==: 2*x + (1/2)
      , -(1/2)*x + (3/4)       :==: (5/2)*x + 3
      , -(1/2)*x + 1+(1/2)     :==: 2*x - 5
      , -(1/3)*x + (3/4)       :==: (1/4)*x + (1/6)
      , (3/4)*x - (1/3)        :==: (2/3)*x - (3/4)
      , (2/5)*x - (1/4)        :==: (1/2)*x + (3/4)
      , (2/3)*x - 2            :==: (1/5)*x - (3/5)
      , (-1+(2/5))*x + 3+(1/2) :==: (3/5)*x + (9/10)
      ]

   level5 :: [Equation Expr]
   level5 = 
      let x = variable "x" in
      [ (1/4)*(x-3)         :==: (1/2)*x - 4
      , (x+3)/2             :==: 5*((1/2)*x + 1 + (1/2))
      , (1/2)*(7-(2/3)*x)   :==: 2 + (1/9)*x
      , (3/4)*x - (x-1)     :==: 3 + (2+(1/2))*(x-1)
      , -(5/4)*(x-7)        :==: (3/4)*(x+2) - (4+(1/2))
      , 3*((1/5)*x - 1) + 5 :==: 7*x - 14
      , ((5*x - 1) / 6) - 2 :==: -4*x + (3*x - 6)/2
      , 2*x - ((2*x+2)/5)   :==: 12 + (x-1)/6
      , (-3*(x+2))/6        :==: 9*((2/3)*x + (1/3)) - (5/3)
      , 1 - ((4*x + 2)/3)   :==: 3*x - ((5*x - 1) / 4)
      ]

quadraticEquations :: [[Equation Expr]]
quadraticEquations = [level1, level2, level3, level4, level5, level6]
 where
   level1 = 
      let x = variable "x" in
      [ x^2            :==: 2
      , x^2+3          :==: 52
      , x^2-7          :==: 0
      , 9*x^2 - 6      :==: 75
      , 32 - 2*x^2     :==: 14
      , 2*(x^2 - 3)    :==: 12
      , (1/4)*x^2 + 12 :==: 16
      , (x-1)^2        :==: 100
      , 14 - 2*x^2     :==: 6
      , (1/4)*(17-x^2) :==: 2
      ] 
   
   level2 = 
      let x = variable "x" in
      [ (x-7)^2 + 3      :==: 11
      , (6-2*x)^2        :==: 81 
      , (1/2)*(x+9)^2    :==: 4
      , (3-x^2)/10       :==: 2
      , 5*x^2 + 3*x      :==: 3*x + 2
      , 11 - (2*x + 1)^2 :==: 5
      , (6*x - 3)^2 + 6  :==: 12
      , (7+2*x)^2        :==: 7
      , 4 - (x^2 / 10)   :==: 6
      , 12 - (2*x + 3)^2 :==: 6
      ]
   
   level3 = 
      let x = variable "x" in
      [ x^2 :==: 5*x
      , x^2 - 6*x     :==: 0
      , 6*x + x^2     :==: 0
      , x*(x+4)       :==: 0
      , x*(2*x-4)     :==: 0
      , 3*x^2         :==: 6*x
      , 3*x           :==: 2*x^2
      , x*(1-6*x)     :==: 0
      , (x+5)*(x-8)   :==: 0
      , (3*x-1)*(x+3) :==: 0
      ]
      
   level4 = 
      let x = variable "x" in
      [ x^2-2*x     :==: 3
      , x^2+12*x+20 :==: 0
      , x^2-x       :==: 30
      , x*(x+2)     :==: 8
      , x*(x-3)     :==: 4
      , 2*x+15      :==: x^2
      , 4*x         :==: 12 - x^2
      , x^2         :==: 15 - 8*x
      , x^2-9*x+18  :==: 0
      , x^2+14*x+24 :==: 0
      ]
      
   level5 = 
      let x = variable "x" in
      [ (3*x+5)^2+(x-5)^2 :==: 40 
      , 4*(10-x^2)        :==: -2*x*(2*x + 10)
      , x*(x+12)          :==: 2*x^2
      , 3*(x-2)*(x+6)     :==: 12*x
      , 8*x^2+4*x-24      :==: (x+3)*(x-8)
      , 3*x^2 - 11        :==: (3+2*x)^2
      , 2*x*(x-3)-3       :==: (x+2)*(x+6)
      , 12*(x^2-3*x)+8    :==: 56
      , 4*x^2-6*x         :==: x^2+9
      , (x+1)*(x-5)       :==: (x-2)*(x-3)
      ]
   
   level6 = 
      let x = variable "x" in
      [ x^2+4*x-4   :==: 0
      , x^2-6*x     :==: 4
      , x^2-12*x+34 :==: 0
      , 2*x^2+4*x-8 :==: 0
      , (x-4)*(x-1) :==: 11
      , (x-(7/2))^2 :==: 2*(x+4)
      , x^2-3*x     :==: 3*(x-2)
      , (4-x)*(1-x) :==: 3*x
      , 2*x^2       :==: x*(x+2)+7
      , (1-x)^2     :==: x+2
      ]

-- Ontbinden in factoren (VWO B, hoofdstuk 1)
findFactors :: [[Expr]]
findFactors =
   let x = variable "x" in
   [ -- (buiten haakjes brengen)
     [ 4*x^2 -4*x
     , 36*x^2+30*x
     , -6*x^2-18*x
     , 14*x^2-10*x
     ] --(product-som methode)
   , [ x^2+11*x+24
     , x^2-8*x+15
     , x^2-x-2
     , x^2-11*x+28
     ]
   ]

modulusEquations :: [[Equation Expr]]
modulusEquations = 
   let x = variable "x" in
   [ [ abs (2*x + 3)  :==: 2 
     , abs (5 - 2*x)  :==: 1
     , abs (4*x + 7)  :==: 3
     , abs (11 - 3*x) :==: 15
     ]
   , [ abs (x^2 + 3) :==: 9
     , abs (x^2 - 7) :==: 2
     , abs (3-x^2)   :==: 6
     , abs (9-x^2)   :==: 7
     ]
   , [ abs (2*x^2+3)     :==: 19
     , abs (3*x^2-2)     :==: 1
     , abs (6-2*x^2)     :==: 2
     , abs (3-(1/2)*x^2) :==: 15
     ]
   , [ abs (4*x^3-72)       :==: 36
     , abs (0.2*x^4 - 112)  :==: 13
     , abs (2*x^4-4.25)     :==: 3.75
     , abs (1.75 - 0.5*x^3) :==: 2.25
     ]
   ]

sqrtEquations :: [[Equation Expr]]
sqrtEquations = 
   let x = variable "x" in
   [ [ 5 - 2*sqrt x   :==: 1
     , 7 - 3 * sqrt x :==: 5
     , 4 - 2*sqrt x   :==: -3
     , 6 - 3*sqrt x   :==: 2
     ]
   , [ 2*sqrt x       :==: x
     , 2*sqrt x       :==: 3*x
     , x-3*sqrt x     :==: 0
     , 3*x - 5*sqrt x :==: 0
     ]
   , [ x :==: sqrt (2*x + 3)
     , x :==: sqrt (3*x+10)
     , x :==: sqrt (4*x + 21)
     , x :==: sqrt (3*x + 4)
     ]
   , [ 5*x :==: sqrt (50*x + 75)
     , 2*x :==: sqrt (24*x + 28)
     , 3*x :==: sqrt (27*x - 18)
     , 2*x :==: sqrt (28*x - 40)
     , 3*x :==: sqrt (3*x + 42)
     , 5*x :==: sqrt (49*x + 2)
     , 3*x :==: sqrt (10*x -1)
     , 5*x :==: sqrt (30*x - 5)
     ]
   , [ x - sqrt x     :==: 6
     , x - 4*sqrt x   :==: 12
     , x - sqrt x     :==: 12
     , x - sqrt x     :==: 2
     , 2*x + sqrt x   :==: 3
     , 3*x + 4*sqrt x :==: 20
     , 2*x + sqrt x   :==: 15
     , 2*x - 3*sqrt x :==: 27 
     ]
   ]
   
sqrtSubstEquations :: [[Equation Expr]]
sqrtSubstEquations = 
   let x = variable "x" in
   [ [ 8*x^3 + 1 :==: 9*x * sqrt x
     , 27*x^3    :==: 28*x*sqrt x - 1
     , x^3 + 3   :==: 4*x*sqrt x
     , x^3       :==: 10*x*sqrt x - 16
     ]
   , [ x^3               :==: 6*x*sqrt x + 16
     , x^3 - 24*x*sqrt x :==: 81
     , x^3 + x*sqrt x    :==: 20
     , x^3 - 15          :==: 2*x*sqrt x
     ]
   , [ x^5 +32                      :==: 33*x^2*sqrt x
     , 243*x^5 - 244*x^2*sqrt x + 1 :==: 0
     , 32*x^5 + 31*x^2*sqrt x       :==: 1
     , x^5                          :==: 242*x^2*sqrt x + 243
     ]
   , [ x^5 + 8              :==: 6*x^2*sqrt x
     , x^5                  :==: 9*x^2 * sqrt x - 18
     , x^5                  :==: 5*x^2*sqrt x + 24
     , x^5 + 4*x^2 * sqrt x :==: 12
     ]
   ]
   
brokenEquations :: [[Equation Expr]]
brokenEquations = 
   let x = variable "x" in
   [ [ (2*x^2 - 10) / (x^2+3) :==: 0 
     , (7*x^2 - 21) / (2*x^2 - 5) :==: 0
     , (3*x^2 - 6) / (4*x^2+1) :==: 0
     , (4*x^2 - 24) / (6*x^2 - 2) :==: 0
     , x^2 / (x+4) :==: (3*x+4) / (x+4)
     , (x^2 + 2) / (x-2) :==: (x+8) / (x-2)
     , (x^2 + 6*x - 6) / (x^2 - 1) :==: (4*x + 9) / (x^2 - 1)
     , (x^2 + 6) / (x^2 - 2) :==: (7*x) / (x^2 - 2)
     ]
   , [ (x^2 + 6*x) / (x^2 - 1) :==: (3*x + 4) / (x^2 - 1)
     , (x^2 + 6) / (x - 3) :==: (5*x) / (x - 3)
     , (x^2 + 4*x) / (x^2 - 4) :==: (3*x + 6) / (x^2 - 4)
     , (x^2 + 2*x - 4) / (x-5) :==: (4*x + 11) / (x - 5)
     , (5*x+2) / (2*x - 1) :==: (5*x + 2) / (3*x + 5)
     , (x^2-9) / (4*x - 1) :==: (x^2 - 9) / (2*x + 7)
     , (3*x - 2) / (2*x^2) :==: (3*x - 2) / (x^2 + 4)
     , (2*x + 1) / (x^2+3*x) :==: (2*x + 1) / (5*x + 8)
     ]
   , [ (x^2 - 1) / (2*x + 2) :==: (x^2-1) / (x+8)
     , (x^2 - 4) / (3*x - 6) :==: (x^2-4) / (2*x+1)
     , (x^2 + 5*x) / (2*x^2) :==: (x^2 + 5*x) / (x^2 + 4)
     , (x^2 - 3*x) / (2*x - 6) :==: (x^2 - 3*x) / (4*x + 2)
     , x/(x+1) :==: 1+3/4
     , (x+2)/(3*x) :==: 1+1/3
     , (2*x+3) / (x-1) :==: 3+1/2
     , (x-3)/(1-x) :==: 1+2/5
     ]
   , [ (x+4)/(x+3) :==: (x+1)/(x+2)
     , (2*x+3)/(x-1) :==: (2*x-1) / (x-2)
     , (3*x+6)/(3*x-1) :==: (x+4) / (x+1)
     , (x+2)/(2*x+5) :==: (x+4)/(2*x-3)
     , (x+5)/(2*x) +2 :==: 5
     , (3*x+4)/(x+2) - 3 :==: 2
     , (x^2)/(5*x+6) +4 :==: 5
     , (x^2)/(2*x-3) + 3 :==: 7
     ]
   , [ (x-2) / (x-3) :==: x/2
     , (x+9) / (x-5) :==: 2/x
     , (x+2) / (x+4) :==: 2/(x+1)
     , (-3) / (x-5) :==: (x+3)/(x-1)
     , (x+1)/(x+2) :==: (7*x+1) / (2*x-4)
     , (2*x-7)/(5-x) :==: (x+1) / (3*x-7)
     , (x+1)/(x-1) :==: (3*x-7)/(x-2)
     , (3*x-7)/(x-2) :==: (7-x) / (3*x-3)
     ]
   ]
   
infix 9 *|

-- short-hand notation for a product with a square-root
(*|) :: Expr -> Expr -> Expr
a *| b = a * sqrt b

simplerSqrt :: [[Expr]]
simplerSqrt = 
   let a = Var "a" in
   [ [ 9*|5 * 7*|3, 3*|2 * 2*|5, 5*|2 * 6*|7, 4*|6 * 2*|7, (6*a)*|3 * 9*|2
     , 5*|5 * (2*a)*|7, a*|6 * 7*|5, 8*|7 * a*|3
     ]
   , [ sqrt 15 / 6*|3, 5*|30/sqrt 5, 4*|10 / 5*|2, 5*|21 / 2*|7, (6*a)*|35 / 3*|5
     , (5*a)*|14 / 9*|2, a*|6 / 7*|3, (a*3)*|42 / 7*|7
     ]
   , [ 5/2*|2, 2/5*|3, 3/2*|5, 8/7*|6, (2*a)/3*|7, (6*a)/7*|10, (5*a)/3*|11
     , (6*a)/5*|13
     ]
   , [ sqrt (2/3), sqrt (5+1/3), sqrt (1+1/2), sqrt (3+4/7), sqrt (5*a^2)
     , sqrt (7*a^2), sqrt (3*a^2), sqrt (6*a^2)
     ]
   , [ sqrt ((2/9)*a^2), sqrt ((5/16)*a^2), sqrt ((3/25)*a^2), sqrt ((7/16)*a^2)
     , ((1/3)*|2)^2, ((1/2)*|3)^2, ((2/7)*|5)^2, ((2/3)*|7)^2
     ]
   ]
   
simplerSqrt2 :: [[Expr]]
simplerSqrt2 = 
   let a = Var "a" in
   [ [ (((1/7)*a)*|2)^2, (((3/5)*a)*|3)^2, (((1/3)*a)*|5)^2, (((4/7)*a)*|6)^2
     , sqrt 8 + sqrt 2, sqrt 2 + sqrt 18, sqrt 12 - sqrt 3, sqrt 7 - sqrt 28
     ]
   , [ sqrt 12 + sqrt 48, sqrt 18 - sqrt 8, sqrt 45 - sqrt 20, sqrt 80 + sqrt 45
     , sqrt (50*a^2) - sqrt (32*a^2), sqrt (75*a^2) - sqrt (12*a^2)
     , sqrt (27*a^2) + sqrt (3*a^2), sqrt (24*a^2) + sqrt (96*a^2)
     ]
   , [ sqrt 27 + 1 / sqrt 3, sqrt 24 + 5/sqrt 6, sqrt 72- 7/ sqrt 2
     , sqrt 98 - 5/sqrt 2, sqrt 24 + sqrt (1+1/2), sqrt 40 - sqrt (2+1/2)
     , sqrt 75 - sqrt (1+1/3)
     , sqrt (1+2/3) + sqrt 60
     ] 
   ]
   
simplerSqrt3 :: [[Expr]]
simplerSqrt3 = 
   let a = Var "a" in
   [ [ (2*|7 + 7*|3)^2, (sqrt 2 + 6*|3)^2, (4*|3 + 3*|2)^2, (2*|5 + sqrt 7)^2
     , (3*|6 - 4*|5)^2, (5*|3 - sqrt 2)^2, (4*|6 - 2*|7)^2, (sqrt 5 - 2*|3)^2
     ]
   , [ (2*|3 - 2)^2, (5*|2 - 1)^2, (3+4*|3)^2, (2+3*|6)^2, (4*|2+3)*(4*|2 - 3)
     , (sqrt 7 + sqrt 3)*(sqrt 7 - sqrt 3), (2*|2 - sqrt 5)*(2*|2 + sqrt 5)
     , (6-3*|3)*(6+3*|3)
     ]
   , [ (a-sqrt 3)^2,  (2*|6 + a)^2, (2*a + a*|5)^2, (a*|3 - (2*a)*|2)^2
     , (a-sqrt 7)*(a+sqrt 7), (3*a + 2*|3)*(3*a - 2*|3)
     , (2*a + a*|2)*(2*a - a*|2), ((3*a)*|5 - a)*((3*a)*|5 + a)
     ]
   , [ 4/(sqrt 2 + 2), 3/(sqrt 5 + 1), 2/(sqrt 3 - 3), 5/(sqrt 6 - 2)
     , 6/(sqrt 7 + sqrt 5), 4/(2*|3 + sqrt 6), 5/(3*|2 - sqrt 3)
     , 2/(sqrt 11-sqrt 2)
     ]
   , [ 2*|3 / (sqrt 5 + sqrt 2), 6*|5 / (sqrt 7 + sqrt 3)
     , 4*|3 / (sqrt 5 - sqrt 3), 8*|7 / (sqrt 6 - sqrt 5)
     ]
   ]