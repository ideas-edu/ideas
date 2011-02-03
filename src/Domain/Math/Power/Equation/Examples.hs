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
module Domain.Math.Power.Equation.Examples
   ( powerEquations, expEquations, logEquations, higherPowerEquations
   , rootEquations, rootEquations2, rootSubstEquations, expEquations2
   ) where

import Prelude hiding ((^))
import Domain.Math.Data.Relation
import Domain.Math.Expr

----------------------------------------------------------
-- HAVO B applets

-- Hoofdstuk 7, vergelijkingen met machten algebraisch (6)
powerEquations :: [[Equation Expr]]
powerEquations = 
  -- los vergelijkingen algebraisch op
  let x = Var "x" in
  [ [ x^14 :==: 25
    , x^(-7) :==: 110
    , 2*x^(3.5) :==: 70
    , 8*x^(-(9.2)) :==: 1000
    ]
  , [ root x 5 :==: 2.9
    , 5 * root x 3 :==: 7
    , root (x^3) 4 :==: 720
    , root (x^2) 5 :==: 5.5
    ]
  , [ 4*x^(-12) :==: 28 
    , 7*x^(5.1) + 16 :==: 100
    , 8*x^(-((1.9))) - 5 :==: 2
    , 0.8 * x^(0.7) + 7 :==: 12.5
    ]
  , [ 4*root x 7 + 7 :==: 11.8
    , 9*x^(3.2)+17 :==: 37
    , 6*x^(-(3.1))-9 :==: 12
    , 0.7 * x^(-(1.1)) + 17 :==: 40
    ]
  ]

-- Hoofdstuk 7, exponentiele vergelijkingen algebraisch (7)
expEquations :: [[Equation Expr]]
expEquations =
  -- los exponentiele vergelijkingen algebraisch op
  let x = Var "x" in
  [ [ 2^x :==: 16 * sqrt 2
    , 2^(x+2) :==: 1/4
    , 3^(x-1) :==: 81
    , 3^(x+5) :==: 243/(sqrt 3)
    ]
  , [ 5^(2-x) :==: 0.04
    , 3^(2*x) :==: 1/9
    , 3^(1-3*x) :==: 81
    , 3^(3*x-2) :==: 3*sqrt 3
    ]
  , [ 5*2^(x-1) :==: 20*sqrt 2
    , 6*5^(2-x) :==: 150
    , 2*7^(4*x-1) :==: 98
    , 8*3^(5-2*x) :==: 72*sqrt 3
    ]
  , [ 2^x-7 :==: 9
    , 4^(3*x)+5 :==: 69
    , 7*3^(2*x+1) :==: 189
    , 5*2^(1-4*x)+11 :==: 51
    ]
  , [ 5^(x-4) :==: (1/5)^(2*x+1)
    , 7^(1-2*x) :==: 1
    , 4^(2*x-3) :==: 2*sqrt 2
    , 2*9^(1-2*x) :==: 6*sqrt 3
    ]
  ]

-- Hoofdstuk 7, logaritmische vergelijkingen algebraisch (8)
logEquations :: [[Equation Expr]]
logEquations =
  -- los algebraisch op
  let x = Var "x" in
  [ [ logBase 2 x :==: 7
    , logBase 3 (x-2) :==: 2
    , logBase 4 (x-3) :==: 1+(1/2)
    , logBase 5 ((1/10)*x-3) :==: -1
    , logBase x 7 :==: 1
    , logBase x 4 :==: -1
    , logBase 2 (x^2-1) :==: 3
    , logBase (1/3) (1-5*x) :==: -1
    ]
  ]


----------------------------------------------------------
-- VWO A/C applets

-- Hoofdstuk 5, hogeremachtswortels (1)
higherPowerEquations :: [[Equation Expr]]
higherPowerEquations =
  -- bereken exacte oplossing
  let x = Var "x" in
  [ [ 2*x^3+9 :==: 19
    , 4*x^5-17 :==: 27
    , 3*x^7+8 :==: 62
    , 5*x^3-1 :==: 9
    , 6-5*x^3 :==: 76
    , 11-7*x^5 :==: 53
    , 4-(1/5)*x^7 :==: 9
    , 18-11*x^7 :==: 62
    ]
  , [ (1/2)*x^4+5 :==: 12
    , 5*x^6-37 :==: 68
    , 4*x^8-19 :==: 9
    , 5*x^6+7 :==: 97
    , 18-7*x^4 :==: -38
    , 3+(1/3)*x^6 :==: 7
    , 1-(1/9)*x^8 :==: -4
    , 47+15*x^8 :==: 77
    ]
  , [ 18*x^8-11 :==: 7
    , (1/4)*x^6+14 :==: 30
    , 5*x^4+67 :==: 472
    , 5*x^4-1 :==: 4
    , (1/8)*x^7+24 :==: 40
    , (1/5)*x^3+27 :==: 52
    , 32*x^3+18 :==: 22
    , 4*x^3-8 :==: 100
    ]
  , [ 14-2*x^3 :==: 700
    , 4-3*x^5 :==: 100
    , 14-11*x^7 :==: 25
    , 1-3*x^5 :==: 97
    ]
    -- Geef in twee decimalen nauwkeurig
  , [ 3*x^5+7 :==: 15
    , 0.7 * x^4 - 1.3 :==: 2
    , (1/3)*x^7 :==: 720
    ]
  ]

-- Hoofdstuk 5, hogeremachtswortels (2)
rootEquations :: [[Equation Expr]]
rootEquations = 
  -- Bereken exacte oplossing
  let x = Var "x" in
  let y = Var "y" in
  [ [ x^4 :==: 6
    , root x 4 :==: 6
    , sqrt x :==: 10
    , root x 5 :==: 2
    ]
  , [ 3*x^5-1 :==: 20
    , 3*root (x-1) 5 - 1 :==: 20
    , (1/10)*sqrt x + 2 :==: 12
    , (1/5)*x^7+8 :==: 26
    ]
  , [ 3*root x 4+2 :==: 14
    , (1/2)*x^8-2 :==: 18
    , 5-2*root x 3 :==: 3
    ]
  -- Maak x vrij
  , [ y :==: x^5
    , y :==: 2*x^5+4
    , y :==: (1/10)*x^3-6
    , y :==: root x 7
    , y :==: 2*root x 3+8
    , y :==: (1/10)*root x 5-6
    ]
  , [ y :==: 3*root x 7-6
    , y :==: (1/4)*x^9-6
    , y :==: 8+(1/2)*root x 3
    ]
  ]



----------------------------------------------------------
-- VWO B applets

-- Hoofdstuk 1, wortelvergelijkingen
rootEquations2 :: [[Equation Expr]]
rootEquations2 =
  let x = Var "x" in
  -- los algebraisch op
  [ [ 5-2*sqrt x :==: 1
    , 7-3*sqrt x :==: 5
    , 4-2*sqrt x :==: -3
    , 6-3*sqrt x :==: 2
    ]
  , [ 2*sqrt x :==: x
    , 2*sqrt x :==: 3*x
    , x-3*sqrt x :==: 0
    , 3*x-5*sqrt x :==: 0
    ]
  , [ x :==: sqrt (2*x+3)
    , x :==: sqrt (3*x+10)
    , x :==: sqrt (4*x+21)
    , x :==: sqrt (3*x+4)
    ]
  , [ 5*x :==: sqrt (50*x+75)
    , 2*x :==: sqrt (24*x+28)
    , 3*x :==: sqrt (27*x-18)
    , 2*x :==: sqrt (28*x-40)
    , 3*x :==: sqrt (3*x+42)
    , 5*x :==: sqrt (49*x+2)
    , 3*x :==: sqrt (10*x-1)
    , 5*x :==: sqrt (30*x-5)
    ]
  , [ x-sqrt x :==: 6
    , x-4*sqrt x :==: 12
    , x-sqrt x :==: 12
    , x-sqrt x :==: 2
    , 2*x+sqrt x :==: 3
    , 3*x+4*sqrt x :==: 20
    , 2*x+sqrt x :==: 15
    , 2*x-3*sqrt x :==: 27
    ]
  ]

-- Hoofdstuk 1, wortelvergelijkingen
rootSubstEquations :: [[Equation Expr]]
rootSubstEquations =
  let x = Var "x" in
  -- los algebraisch op
  [ [ 8*x^3+1 :==: 9*x*sqrt x
    , 27*x^3 :==: 28*x*sqrt x-1
    , x^3+3 :==: 4*x*sqrt x
    , x^3 :==: 10*x*sqrt x-16
    ]
  , [ x^3 :==: 6*x*sqrt x+16
    , x^3-24*x*sqrt x :==: 81
    , x^3+x*sqrt x :==: 20
    , x^3-15 :==: 2*x*sqrt x
    ]
  , [ x^5+32 :==: 33*x^2*sqrt x
    , 243*x^5-244*x^2*sqrt x+1 :==: 0
    , 32*x^5+31*x^2*sqrt x :==: 1
    , x^5 :==: 242*x^2*sqrt x+243
    ]
  , [ x^5+8 :==: 6*x^2*sqrt x
    , x^5 :==: 9*x^2*sqrt x-18
    , x^5 :==: 5*x^2*sqrt x+24
    , x^5+4*x^2*sqrt x :==:12
    ]
  ]

-- Hoofdstuk 5, exponentiele vergelijkingen exact oplossen (1, 2, 2a)
expEquations2 :: [[Equation Expr]]
expEquations2 =
  let x = Var "x" in
  -- los algebraisch op
  -- 1
  [ [ 2^(2*x-1) :==: 1/16
    , 3^(1-x) :==: 81
    , 5^(1-2*x) :==: 1/5
    , (1/2)^(4*x-3) :==: 1/4
    , (1/3)^(5*x+2) :==: 1/3
    , 6^(3*x-2) :==: 1/216
    ]
  , [ 2^(3*x+2) :==: 2*sqrt 2
    , 3^(2*x+1) :==: 9*sqrt 3
    , 5^(4*x+3) :==: 625*sqrt 5
    , (1/2)^(x+1) :==: 4
    , (1/3)^(x-3) :==: 3
    , 4^(x+2) :==: 64*root 4 3
    ]
  , [ 2^(x+3) :==: (1/2)*root 2 3
    , 3^(4*x+1) :==: 27
    , 5^(-x+2) :==: 1/25
    , (1/2)^(1-x) :==: sqrt 2
    , (1/3)^(x+1) :==: (1/9)*sqrt 3
    , 2^(1-3*x) :==: (1/8)*sqrt 2
    ]
  , [ 3*2^x+1 :==: 25
    , 4*3^x-9 :==: 27
    , 2*5^x+4 :==: 14
    , 5*(1/2)^x+11 :==: 51
    , 8*(1/3)^x+27 :==: 99
    , 3*(1/5)^x-35 :==: 40
    ]
  , [ 2^(4*x+3) :==: 1
    , (1/2)^(2*x-1) :==: 1
    , 3^(2*x+4) :==: 1
    , (1/3)^(x-3) :==: 1
    , 4^(4*x-7) :==: 1
    , 5^(3*x-6) :==: 1
    ]
  -- 2
  , [ 2^(2*x+1) :==: (1/2)^(x+2)
    , 4^(2*x-1) :==: 2^(3*x+2)
    , 2^(5*x-4) :==: 8^(x-3)
    , (1/4)^(2*x+1) :==: 2^(6-2*x)
    , (1/3)^(2*x-3) :==: 3^(4*x-3)
    , 3^(3*x-2) :==: 9^(2-x)
    , 27^(2*x+1) :==: 3^(2*x-5)
    , 3^(5*x-1) :==: (1/9)^(2*x-1)
    ]
  , [ 6^(7*x-3) :==: 36^(2*x+3)
    , (1/7)^(2*x-1) :==: 7^(2*x-7)
    , 5^(5-2*x) :==: (1/5)^(x+2)
    , 25^(4*x+1) :==: 5^(5*x-4)
    , 3^(x^2) :==: (1/3)^(2*x)
    , (1/2)^(x^2) :==: 2^(2*x)
    , 5^(x^2) :==: 25^(3*x)
    , 2^(x^2) :==: (1/8)^(-x)
    ]
  , [ (1/2)^(2-2*x) :==: 4^(3*x+5)
    , 8^(x+1) :==: (1/2)^(x+7)
    , (1/4)^(x+2) :==: 8^(2*x-1)
    , 8^(2*x-3) :==: 16^(2*x+3)
    , (1/3)^(x-2) :==: 9^(x+4)
    , 9^(2*x-1) :==: 27^(2*x-1)
    , (1/9)^(x+3) :==: 27^(2*x+2)
    , 27^(3-2*x) :==: (1/3)^(4*x+3)
    ]
  , [ 4*2^x :==: 2^(3*x-2)
    , 2^(5*x-9) :==: (1/8)*2^x
    , 3^(4*x+6) :==: 27*3^x
    , (1/9)*3^x :==: 3^(2-3*x)
    , 3*3^x :==: (1/3)^(2*x+5)
    , 4^(x+1) :==: 8*2^x
    , (1/2)*2^x :==: (1/2)^x
    , 9^(x+2) :==: (1/3)*3^x
    ]
  , [ (1/5)*5^(3*x-2) :==: 25^(x+1)
    , 9*3^(2*x+1) :==: (1/3)^(4*x-3)
    , 4^(3*x-5) :==: 8*2^(x+2)
    , (1/2)^(3-2*x) :==: (1/4)*2^(3*x-4)
    , 2^(x+2)+2^x :==: 40
    , 2^(x+4) :==: 3/4+2^(x+2)
    , 2^(x-2)+2^(x+1) :==: 9
    , 2^(x+5)-2^(x+4) :==: 16
    ]
  -- 2a
  , [ 3^(x+2) :==: 72+3^x
    , 3^(x-1)+3^(x+1) :==: 10
    , 3^(x+3)+3^(x+2) :==: 12
    , 3^x-3^(x-1) :==: 54
    ]
  , [ 5^(x+1)+5^x :==: 150
    , 5^(x+1) :==: 100+5^x
    , 5^(x+2)+5^x :==:1+1/25
    , 5^(x+1)+5^(x+2) :==: 30
    ]
  , [ 2^(x+4)-2^(x-2) :==: 63*sqrt 2
    , 3^(x-1)+3^x :==: 12*sqrt 3
    , 5^x-5^(x-1) :==: 4*sqrt 5
    , 2^(x+2)+2^(x-3) :==: 66*sqrt 2
    ]
  ]