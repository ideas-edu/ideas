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
module Domain.Math.Polynomial.Examples 
   ( linearExamples, quadraticExamples, higherDegreeExamples
   , factorizeExamples, expandExamples
   , ineqLin1, ineqQuad1, ineqQuad2, extraIneqQuad, ineqHigh
   ) where

import Prelude hiding ((^))
import Common.Exercise
import Common.Rewriting
import Domain.Math.Expr
import Domain.Math.Data.Relation

x :: Expr
x = variable "x"

linearExamples :: Examples (Equation Expr)
linearExamples =
   level Easy -- applet level 1
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
      ] ++
   level Easy -- applet level 2
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
      ] ++
   level Medium -- applet level 3
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
      ] ++
   level Medium -- applet level 4
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
      ] ++
   level Medium -- applet level 4
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

quadraticExamples :: Examples (Equation Expr)
quadraticExamples =
   level Easy -- applet level 1
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
      ] ++
   level Medium -- applet level 2
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
      ] ++
   level Medium -- applet level 3
      [ x^2           :==: 5*x
      , x^2 - 6*x     :==: 0
      , 6*x + x^2     :==: 0
      , x*(x+4)       :==: 0
      , x*(2*x-4)     :==: 0
      , 3*x^2         :==: 6*x
      , 3*x           :==: 2*x^2
      , x*(1-6*x)     :==: 0
      , (x+5)*(x-8)   :==: 0
      , (3*x-1)*(x+3) :==: 0
      ] ++
   level Medium -- applet level 4
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
      ] ++
   level Difficult -- applet level 5
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
      ] ++
   level Difficult -- applet level 6
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

factorizeExamples :: Examples Expr
factorizeExamples = 
   level Easy 
      [ -- (buiten haakjes brengen)
       4*x^2 -4*x
      , 36*x^2+30*x
      , -6*x^2-18*x
      , 14*x^2-10*x
        --(product-som methode)
      , x^2+11*x+24
      , x^2-8*x+15
      , x^2-x-2
      , x^2-11*x+28
      ]

expandExamples :: Examples Expr
expandExamples = level Easy $
   [ 5*(x+1), -3*(x-3), (x-1)*7
   , 4*(3-2*x), (x+1)*(x-3), (x+1)*(1-x)
   , x*(x-1), 3*(x-2)*2*x
   , (x-1)^2, (x+1)^2, (x-1)^2*(x+1)
   , (x+1)^3, (x-1)^3*x, (x-1)*(x+3)*(x-5)
   , x/2, (x+1)/2, (x+1)/2 + (x+2)/3, ((x+1)/2) * ((x+2)/3)
   ]

--------------------------------------------------------------------
-- Algemene applet

higherDegreeExamples :: Examples (Equation Expr)
higherDegreeExamples =
   -- Havo B hoofdstuk 3, Hogeregraadsvgl.
   level Easy
      [ -- level 1
        (1/3)*x^3 :==: 9 
      , x^5 - 12 :==: 20
      , 1 - 8*x^3 :==: -124
      , 16 - 32*x^5 :==: - 227
        -- level 2
      , 3*x^4 :==: 48 
      , (1/9)*x^6 + 12 :==: 93
      , 39 - 8*x^2 :==: 21
      , (1/2)*x^4 - 13 :==: 27.5
        -- level 3
      , 3*(2*x-1)^3 + 11 :==: 659 
      , 0.5*(3*x-4)^5 + 7 :==: 23
      , 2*(0.5*x+3)^7 - 11 :==: -9
      , 5*(1-4*x)^3 + 4 :==: -621
        -- level 4
      , 3*(2*x + 5)^2 + 9 :==: 21
      , 2*(3*x-6)^6 - 24 :==: -22
      , -2*(4*x-5)^4 + 192 :==: -8000
      , (3-2*x)^4 + 23 :==: 279
      ] ++
   level Easy 
      [ -- level 1
        2*x^3 + 9 :==: 19
      , 4*x^5 - 17 :==: 27
      , 3*x^7 + 9 :==: 62
      , 5*x^3 - 1 :==: 9
      , 6 - 5*x^3 :==: 76
      , 11 - 7*x^5 :==: 53
      , 4 - 0.2*x^7 :==: 9
      , 18 - 11*x^7 :==: 62
        -- level 2
      , 0.5*x^4 + 5 :==: 12
      , 5*x^6 - 37 :==: 68
      , 4*x^8 - 19 :==: 9
      , 5*x^6 + 7 :==: 97
      , 18 - 7*x^4 :==: -38
      , 3 + (1/3)*x^6 :==: 7
      , 1 - (1/9)*x^8 :==: -4
      , 47 + 15*x^8 :==: 77
        -- level 3
      , 18*x^8 - 11 :==: 7
      , (1/4)*x^6 + 14 :==: 30
      , 5*x^4 + 67 :==: 472
      , 5*x^4 - 1 :==: 4
      , (1/8)*x^7 + 24 :==: 40
      , 0.2*x^3 + 27 :==: 52
      , 32*x^3 + 18 :==: 22
      , 4*x^3 - 8 :==: 100
        -- level 4
      , 14 -2*x^3 :==: 700
      , 4-3*x^5 :==: 100
      , 14 - 11*x^7 :==: 25
      , 1 - 3*x^5 :==: 97
      , 3*(x-2)^4 + 7 :==: 37
      , 6 - (2*x-1)^3 :==: 1
      , (1/3)*(x+5)^6 - 4 :==: 3
      , 6 - 0.5*(x-1)^5 :==: 10
        -- level 5
      , (1/2)*(3*x-1)^4 :==: 8
      , 100-(1/3)*(4*x-3)^5 :==: 19
      , 4*(0.5*x+2)^6 + 5 :==: 9
      , 3*(2*x + 7)^3 + 11 :==: 35
      ] ++
   level Medium 
      -- (Ontbinden applet)
      [ -- level 1
        x^3 - 5*x^2 + 4*x :==: 0
      , x^3 :==: 3*x^2 + 10*x
      , 14*x :==: x^3 + 5*x^2
      , (1/2)*x^3 + 3*x^2 + 4*x :==: 0
      , x^3 + 6*x^2 + 9*x :==: 0
      , 5*x^2 :==: x^3 + 6*x
      , x^3 - 5*x^2 :==: 6*x
      , x^3 :==: 4*x^2 + 12*x
        -- level 2
      , x^4 + 36 :==: 13*x^2
      , x^4 - 9*x^2 + 20 :==: 0
      , x^4 :==: 2*x^2 + 3
      , x^4 + 2*x^2 :==: 24
      , 7*x^2 + 18 :==: x^4
      , x^4 :==: x^2 + 12
      , 29*x^2 :==: x^4 + 100
      , 2*x^4 + 2*x^2 :==: 12
        -- (abc-form applet)
        -- level 1
      , 4*x^4 + 4 :==: 17*x^2
      , 16*x^4 + 225 :==: 136*x^2
      , 2*x^4 - 15*x^2 + 25 :==: 0
      , 9*x^4 - 28*x^2 + 3 :==: 0
      , 3*x^4 - 14*x^2 - 5 :==: 0
      , 2*x^4 :==: x^2 + 3
      , 9*x^4 + 14*x^2 :==: 8
      , 4*x^4 - 29*x^2 - 24 :==: 0
        -- level 2
      , 8*x^6 - 9*x^3 + 1 :==: 0
      , 27*x^6 + 8 :==: 217*x^3
      , 2*x^6 + x^3 - 1 :==: 0
      , 8*x^6 + 31*x^3 :==: 4
      , 3*x^6 - 80*x^3 - 27 :==: 0
      , 5*x^6 :==: 39*x^3 + 8
      , 7*x^6 + 8*x^3 + 1 :==: 0
      , 4*x^6 + 2 :==: -9*x^3
      ] ++
   level Difficult
      [ x^3 + x^2 :==: 0
      , x^3 - 5*x :==: 0
      , x^3 - 11*x^2 + 18*x :==: 0
      , x^3 + 36*x :==: 13*x^2
      , x^3 + 2*x^2 :==: 24*x
      , 7*x^3 :==: 8*x^2
      , x^4 :==: 9*x^2
      , 64*x^7 :==: x^5
      , x^3 - 4*x^2 - 9*x :==: 0
      , (x-1)*(x^3 - 6*x) :==: 3*x^3 - 3*x^2
      ]

--------------------------------------------------------------------
-- Havo applets

-- Havo B Voorkennis: lineaire ongelijkheden
ineqLin1 :: [[Inequality Expr]]
ineqLin1 =
   let a = Var "a" in
   [ [ 7*x - 12 :<: 5*x + 3
     , 4*(x-3) :>: 3*(x-4)
     , 6*(a+1) :<: 3*(a-2)+4
     , 5 - 2*(a-3) :>: 5*(3-a)
     ]
   , [ 4*x+5 :<: 5*x - 3
     , (1/3)*x+10 :>: (1/2)*x
     , 3*x+1 :<: 7*x + 5
     , x+6 :>: 2 - (3/4)*x
     ]
   , [ 5*(x-1) :<: 7*x - 1
     , -3*(4*x-1) :>: 2-(x-1)
     , 2*(3*x-1) :<: 5-(2-9*x)
     , 2*(x-1)-3*(x-2) :>: 6
     ]
   ]

-- Havo B Voorkennis: kwadratische ongelijkheden 
-- (door eerst gelijkheid op te lossen)
-- (level 2 uit Hoofdstuk 3)
ineqQuad1 :: [[Inequality Expr]]
ineqQuad1 =
   [ [ x^2 +3*x-4 :<: 0
     , x^2-4*x-12 :>: 0
     , -x^2 - 4*x + 5 :<: 0
     , -x^2 + 3*x + 18 :>: 0
     , (1/2)*x^2 - 3*x - 8 :<: 0
     , -2*x^2 + 10*x :>: 0
     ]
   , [ x^2 + 9*x :<: 3*x - 5
     , x^2 - x :>: 12
     , x^2 - 4.5*x :<: 7-3*x
     , 2*x^2 - 10*x :>: x^2 - 3*x
     , 4*x^2 + 6*x :<: x^2 + 3*x + 18
     , 2*x^2 + 6*x - 10 :>: x^2 + 2*x - 5
     ]
   ]

-- Havo B hoofdstuk 3, hogeregraadsongelijkheid exact
-- (door eerst gelijkheid op te lossen)
ineqHigh :: [Inequality Expr]
ineqHigh = 
   [ 2*x^3 :>: 54
   , -0.5*x^4 :<: -40.5
   , 1 - 2*x^5 :<: -485
   , (2*x-3)^4 :>: 1
   , -(0.5*x+2)^3 :<: -1
   , 0.25*(0.5*x-2)^4 :<: 4
   ]

--------------------------------------------------------------------
-- VWO A/C applets

-- hoofdstuk 2
ineqQuad2 :: [Inequality Expr]
ineqQuad2 =
   [ x^2 + 9*x :<: 3*x - 5
   , x^2 - x :>: 12
   , x^2 - 4.5*x :<: 7 - 3*x
   , 2*x^2 - 10*x :>: x^2 - 3*x
   , 4*x^2 + 6*x :<: x^2 + 3*x + 18
   , 2*x^2 + 6*x - 10 :>: x^2 +2*x - 5
   ]

--------------------------------------------------------------------
-- Extra test cases

extraIneqQuad :: [Inequality Expr]
extraIneqQuad = 
   [ x^2-x-7 :>: -100, x^2-x-7 :<: -100, x^2 :<: x^2, x :>=: x 
   , x^2 :>=: 0, x^2 :>: 0, x^2 :<: 0, x^2 :<=: 0
   ]
