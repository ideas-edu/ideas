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
-- Example exercises from the Digital Mathematics Environment (DWO)
--
-----------------------------------------------------------------------------
module Domain.Math.Examples.DWO2 where

import Prelude hiding ((^))
import Domain.Math.Expr
import Domain.Math.Data.Equation
import Domain.Math.Data.Inequality

--------------------------------------------------------------------
-- Algemene applet

higherDegreeEquations :: [Equation Expr]
higherDegreeEquations = 
   let x = variable "x" in
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
   let x = Var "x" in
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
ineqKwad1 :: [[Inequality Expr]]
ineqKwad1 =
   let x = Var "x" in
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
   let x = Var "x" in
   [ 2*x^3 :>: 54
   , -0.5*x^4 :<: -40.5
   , 1 - 2*x^5 :<: -485
   , (2*x-3)^4 :>: 1
   , -(0.5*x+2)^3 :<: -1
   , 0.25*(0.5*x-2)^4 :<: 4
   ]

-- Havo B hoofdstuk 3, Hogeregraadsvgl.
higherEq1 :: [[Equation Expr]]
higherEq1 = 
   let x = Var "x" in
   [ [ (1/3)*x^3 :==: 9
     , x^5 - 12 :==: 20
     , 1 - 8*x^3 :==: -124
     , 16 - 32*x^5 :==: - 227
     ]
   , [ 3*x^4 :==: 48
     , (1/9)*x^6 + 12 :==: 93
     , 39 - 8*x^2 :==: 21
     , (1/2)*x^4 - 13 :==: 27.5
     ]
   , [ 3*(2*x-1)^3 + 11 :==: 659
     , 0.5*(3*x-4)^5 + 7 :==: 23
     , 2*(0.5*x+3)^7 - 11 :==: -9
     , 5*(1-4*x)^3 + 4 :==: -621
     ]
   , [ 3*(2*x + 5)^2 + 9 :==: 21
     , 2*(3*x-6)^6 - 24 :==: -22
     , -2*(4*x-5)^4 + 192 :==: -8000
     , (3-2*x)^4 + 23 :==: 279
     ]
   ]

--------------------------------------------------------------------
-- VWO A/C applets

-- hoofdstuk 2
ineqKwad2 :: [Inequality Expr]
ineqKwad2 =
   let x = Var "x" in
   [ x^2 + 9*x :<: 3*x - 5
   , x^2 - x :>: 12
   , x^2 - 4.5*x :<: 7 - 3*x
   , 2*x^2 - 10*x :>: x^2 - 3*x
   , 4*x^2 + 6*x :<: x^2 + 3*x + 18
   , 2*x^2 + 6*x - 10 :>: x^2 +2*x - 5
   ]
   
--------------------------------------------------------------------
-- VWO B applets

-- hoofdstuk 1
higherEq2 :: [[Equation Expr]]
higherEq2 =
   let x = Var "x" in
   [ [ 2*x^3 + 9 :==: 19
     , 4*x^5 - 17 :==: 27
     , 3*x^7 + 9 :==: 62
     , 5*x^3 - 1 :==: 9
     , 6 - 5*x^3 :==: 76
     , 11 - 7*x^5 :==: 53
     , 4 - 0.2*x^7 :==: 9
     , 18 - 11*x^7 :==: 62
     ]
   , [ 0.5*x^4 + 5 :==: 12
     , 5*x^6 - 37 :==: 68
     , 4*x^8 - 19 :==: 9
     , 5*x^6 + 7 :==: 97
     , 18 - 7*x^4 :==: -38
     , 3 + (1/3)*x^6 :==: 7
     , 1 - (1/9)*x^8 :==: -4
     , 47 + 15*x^8 :==: 77
     ] 
   , [ 18*x^8 - 11 :==: 7
     , (1/4)*x^6 + 14 :==: 30
     , 5*x^4 + 67 :==: 472
     , 5*x^4 - 1 :==: 4
     , (1/8)*x^7 + 24 :==: 40
     , 0.2*x^3 + 27 :==: 52
     , 32*x^3 + 18 :==: 22
     , 4*x^3 - 8 :==: 100
     ] 
   , [ 14 -2*x^3 :==: 700
     , 4-3*x^5 :==: 100
     , 14 - 11*x^7 :==: 25
     , 1 - 3*x^5 :==: 97
     , 3*(x-2)^4 + 7 :==: 37
     , 6 - (2*x-1)^3 :==: 1
     , (1/3)*(x+5)^6 - 4 :==: 3
     , 6 - 0.5*(x-1)^5 :==: 10
     ] 
   , [ (1/2)*(3*x-1)^4 :==: 8
     , 100-(1/3)*(4*x-3)^5 :==: 19
     , 4*(0.5*x+2)^6 + 5 :==: 9
     , 3*(2*x + 7)^3 + 11 :==: 35
     ]
     -- (Ontbinden applet)
    , [ x^3 - 5*x^2 + 4*x :==: 0
      , x^3 :==: 3*x^2 + 10*x
      , 14*x :==: x^3 + 5*x^2
      , (1/2)*x^3 + 3*x^2 + 4*x :==: 0
      , x^3 + 6*x^2 + 9*x :==: 0
      , 5*x^2 :==: x^3 + 6*x
      , x^3 - 5*x^2 :==: 6*x
      , x^3 :==: 4*x^2 + 12*x
      ]
    , [ x^4 + 36 :==: 13*x^2
      , x^4 - 9*x^2 + 20 :==: 0
      , x^4 :==: 2*x^2 + 3
      , x^4 + 2*x^2 :==: 24
      , 7*x^2 + 18 :==: x^4
      , x^4 :==: x^2 + 12
      , 29*x^2 :==: x^4 + 100
      , 2*x^4 + 2*x^2 :==: 12
      ]
      -- (abc-form applet)
    , [ 4*x^4 + 4 :==: 17*x^2
      , 16*x^4 + 225 :==: 136*x^2
      , 2*x^4 - 15*x^2 + 25 :==: 0
      , 9*x^4 - 28*x^2 + 3 :==: 0
      , 3*x^4 - 14*x^2 - 5 :==: 0
      , 2*x^4 :==: x^2 + 3
      , 9*x^4 + 14*x^2 :==: 8
      , 4*x^4 - 29*x^2 - 24 :==: 0
      ]
    , [ 8*x^6 - 9*x^3 + 1 :==: 0
      , 27*x^6 + 8 :==: 217*x^3
      , 2*x^6 + x^3 - 1 :==: 0
      , 8*x^6 + 31*x^3 :==: 4
      , 3*x^6 - 80*x^3 - 27 :==: 0
      , 5*x^6 :==: 39*x^3 + 8
      , 7*x^6 + 8*x^3 + 1 :==: 0
      , 4*x^6 + 2 :==: -9*x^3
      ]
    ]