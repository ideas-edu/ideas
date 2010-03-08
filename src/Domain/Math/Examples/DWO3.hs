-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
module Domain.Math.Examples.DWO3 where

import Prelude hiding ((^))
import Domain.Math.Expr
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols

----------------------------------------------------------
-- HAVO B applets

simplerPowers :: [[Expr]]
simplerPowers = [level1, level2, level3, level4]
 where
   a = variable "a"
   b = variable "b"
   level1 = 
      [ 4*a^3 * 5*a^2
      , 14*a^6 / (-2*a^3)
      , -21*a^7 / (3*a)
      , 5*a * (-3)*a^2 * 2*a^3
      ]
      
   level2 = 
      [ a^2 * (-2*a)^3
      , (2*a)^5 / (-4*a)^2
      , (2*a)^4 * (-3)*a^2
      , (-3*a)^4 / (9*a^2)
      ]
      
   level3 = 
      [ (a^2 * b^3)^7
      , -a^3 * (2*b)^5 * a^2
      , 3*a * (-2*b)^3 * (-a*b)^2
      , (2*a*b^3)^2 * (-3*a^2*b)^3
      ]

   level4 = 
      [ ((1/2)*a)^3 - (4*a)^2 * (1/4)*a
      , (2*a)^5 + ((1/3)*a)^2 * (-3*a)^3
      , (2*a^3)^4 - 6*a^3 * (-a^3)^3
      , (-2*a^3)^2 - 6*(3*a)^2 * (-4*a^4)
      ]

powersOfA :: [[Expr]]
powersOfA = [level1, level2, level3, level4]
  where
    a = variable "a"
    level1 =
      [ a^3 * a^(-4)
      , a^4 * (1/a^2)
      , a^(-1) * a^5
      , (1/a^3) * a 
      ]
      
    level2 =
      [ (a^(-2))^3
      , (a^(-3))^4
      , (1/a^6) * a^(-2)
      , (1/a^2) * (1/a^4)
      ]
      
    level3 = 
      [ (a^(-2))^3 * (1/a^4)
      , (1/a^3)^2
      , (a^3)^2 * (1/a)
      , (a^(-2))^(-3) * a^(-4)
      ]
      
    level4 =
      [ (a^(-1))^2 / a^3
      , (a^2)^(-3) / a^(-1)
      , ((a^(-2))^4 / (a^2)^3) * a
      , (1/a^(-3))^4 * (1/a)^3
      ]
      
nonNegExp :: [[Expr]]      
nonNegExp = [level1, level2]
  where
    a = variable "a"
    b = variable "b"
    level1 =
      [ a * b^(-2)
      , a^(-1) * b^2
      , a^(-2) * b^(-3)
      , (1/a^(-3)) * (b^(-2))^2
      ]
      
    level2 =
      [ (1/(a*b)^(-2)) * a * b^(-1)
      , (2*a)^(-1) / (4*b)^(-2)
      , (4*a*b)^(-1) * (b^2)^(-3)
      , (5*a)^(-2) * 10*b^(-1)
      ]

-- schrijf als een macht van x
powersOfX :: [[Expr]]
powersOfX = 
   [ [root x 3, 1/root x 4, sqrt (1/x), (x^2) / (root (x^2) 5)]
   , [sqrt x/(x^2), root (x/(x^3)) 3, x*root x 3, root x 3 * root (1/(x^2)) 4]
   ]
 where
   x = Var "x"
   
-- Schrijf zonder negatieve of gebroken exponenten
nonNegExp2 :: [[Expr]]
nonNegExp2 = 
   [ [ 4^(1/3), 5^(-(1/4)), 5*a^(1/2), 3*a^(-(1/4))]
   , [ 4/(a^(-1)*b^(1/3)), a^(-1)/(8*b^(-(2/3)))
     , 1/(3*a^(2/5)*b^(-1)), 3*a^(1/4)*b^(-(1/2))
     ]
   ]
 where
   a = Var "a"
   b = Var "b"
   
----------------------------------------------------------
-- HAVO A/C applets

-- herleid
powers1 :: [[Expr]]
powers1 =
   [ [ 5*a^2*2*a^4, 3*a^4*9*a^2, a^5*7*a^3, 4*a^2*9*a^7
     , 2*a^4*5*a^3, 3*a*3*a^4, 2*a^7*2*a^4, 7*a^6*4*a
     ]
   , [ 5*a^4*(1/a), 8*a^4*(1/2*a^2), 2*a^6*(6/a^4), a^2*(8/a)
     , (4*a^3)/(a^5), a^7/a^3, (6*a^8)/(2*a^3), (6*a^5)/(2*a^3)
     ]
   , [ (3*a)^3, (4*a^5)^2, (6*a^3)^2, (2*a^7)^3
     , (-a^6)^5, (-2*a^2)^5, (-4*a^3)^2, (-3*a^5)^4
     ]
   , [ 6*a^5+7*a^5-4*a^9, 8*a^2-4*a^2+2*a^4, 3*a^6+6*a^6+7*a^2
     , 5*a-2*a-9*a^6, 5*a+8*a^2+4*a, 6*a^7-5*a^2+a^7
     , 8*a^6+2*a^3-2*a^6, 2*a^3-8*a^5-a^3 
     ] 
   , [ (4*a^3)^2*2*a^4, (-a^5)^3*5*a^6, 4*a^3*(5*a^6)^2
     , 6*a^7*(2*a^4)^3, a^17/((a^3)^5), a^9/((a^3)^2)
     , a^14/((a^2)^4), a^16/((a^5)^3)
     ] 
   ]
 where
   a = Var "a"
   
-- herleid
powers2 :: [[Expr]]
powers2 =
   [ [ 4*a^3*5*a^2, (14*a^6)/(-2*a^3), (-21*a^7)/(3*a)
     , 5*a*(-3*a^2)*(2*a^3)
     ]
   , [ a^2*(-2*a)^3, (2*a)^5/(-4*a)^2
     , (2*a)^4*(-3*(a^2))
     ]
   , [ (a^2*b^3)^7, (-a)^3*(2*b)^5*a^2
     , 3*a*(-2*b)^3*(-a*b)^2, (2*a*b^3)^2*(-3*a^2*b)^3
     ] 
   , [ (2*a^3)^4-6*a^3*(-a^3)^3, (-2*a^3)^2-6*(3*a)^2*(-4*a^4)
     ]
   ]
 where
   a = Var "a"
   b = Var "b"
   
negExp1 :: [[Expr]]
negExp1 = 
   [ [ a^3/a^7, a^6/a^8, a^3/a^4, a^3/a^9, a/a^5
     , (1/a^3)/a, a/a^7, (1/a^2)/a
     ]
   , [ (1/(a^4))/a^6, (1/(a^3))/a^5, (1/a^5)/a^2, 1/(a^4)/a^3
     , 1/a^3, 1/a^5, 1/a^(-4), 1/a^(-6) 
     ]
   , [ a^8/(1/a^2), a^4/(1/a^4), (a^6)/(1/a^5), a^3/(1/a^6)
     , 1/(a^3)/a^(-2), (1/a^7)/a^(-5), (1/a^2)/a^(-9), (1/a^3)/a^(-8)
     ]
   ]
 where
   a = Var "a"
   
negExp2 :: [[Expr]]
negExp2 = 
   [ [ a^3*a^(-4), a^4*(1/a^2), a^(-1)*a^5, (1/a^3)*a]
   , [ (a^(-2))^3,(a^(-3))^4, (1/a^6)*a^(-2), (1/a^2)*(1/a^4)]
   , [ (a^(-2))^3*(1/a^4), (1/a^3)^2, (a^3)^2*(1/a), (a^(-2))^(-3)*a^(-4)]
   , [ (a^(-1))^2/a^3, (a^2)^(-3)/a^(-1), ((a^(-2))^4/(a^2)^3)*a
     , (1/a^(-3))^4*(1/a)^3
     ]
   ]
 where
   a = Var "a"
   
negExp3 :: [[Expr]]
negExp3 = 
   [ [ 4^(-2), 9^(-2), 3^(-3), 2^(-5)
     , (1/4)^(-3), (1/7)^(-2), (1/2)^(-4), (1/3)^(-4)
     ]
   , [ (3/5)^(-1), (6/7)^(-1), (5/8)^(-1), (7/9)^(-1)
     , 5*3^(-2), 7*2^(-5), 6*5^(-2), 4*7^(-2)
     ]
   ]
   
negExp4 :: [[Expr]]
negExp4 = 
   [ [ a*b^(-2), a^(-1)*b^2, a^(-2)*b^(-3), (1/a^(-3))*(b^(-2))^2]
   , [ (1/((a*b)^(-2)))*a*b^(-1), (2*a)^(-1)/(4*b)^(-2)
     , (4*a*b)^(-1)*(b^2)^(-3), (5*a)^(-2) * 10*b^(-1)
     ]
   ]
 where
   a = Var "a"
   b = Var "b"
   
negExp5 :: [[Expr]]
negExp5 = 
   [ [ (1/3)/(6^(-2)), (1/2)/(8^(-2)), (1/8)/4^(-2), (1/10)/5^(-2)
     , 5*10^(-2), 4*10^(-3), 8*10^(-4), 6*10^(-3)
     ]
   , [ 2*a^(-2)*b^2, 4*a^(-5)*b^3, 3*a^2*b^(-1), 5*a*b^(-3)
     , (1/7)*a^(-2), (1/3)*a^(-4), (1/5)*a^(-6), (1/2)*a^(-3)
     ]
   , [ 3*a^(-1), 4*a^(-4), 5*a^(-3), 2*a^(-7)
     , ((2/3)*a)^(-3), ((3/4)*a)^(-2), ((2/5)*a)^(-3), ((5/6)*a)^(-2)
     ]
   , [ (2*a)^(-3)*b^(-4), 4*a^(-2)*(3*b)^(-2), (4*a)^(-3)*7*b^(-5)
     , 9*a^(-7)*(2*b)^(-4), (a^5) / ((2*b)^(-2)), ((2*a)^(-3))/b^2
     , a^(-3)/b^(-3), (4*a)^(-2)/b^(-4)
     ] 
   ]
 where
   a = Var "a"
   b = Var "b"
   
brokenExp1 :: [[Expr]]
brokenExp1 = 
  [ [ 5*a^(1/2), 7*a^(1/3), (2*a)^(1/4), (3*a)^(1/5)
    , 4*a^(2/3), 2*a^(3/4), 3*a^(2/5), 4*a^(3/5)
    ]
  , [ 6*a^(-(1/2)), 4*a^(-(1/3)), 2*(3*a)^(-(1/4)), (3*a)^(-(1/5))
    , 5*a^(-(2/3)), 7*a^(-(3/4)), 6*a^(-(2/5)), 2*a^(-(3/7))
    ]
  , [ (1/2)*a^(1/3)*b^(-(1/2)), (1/7)*a^(-(1/4))*b^(2/3), 4*a^(1/2)*b^(-(1/5))
    , 3*a^(-(3/5))*b^(1/3), (2*a)^(-(2/3)), (6*a)^(-(2/5))
    , (3*a)^(-(3/5)), (2*a)^(-(4/7))
    ]
  , [ a*sqrt a, a^2*root a 3, a^5*root a 4, a^3*root a 7
    , a*root (a^2) 3, a^3*root (a^2) 5, a^2*root (a^3) 5, a^4*root (a^5) 6
    ] 
  , [ 1/sqrt a, a/root a 3, a^2/sqrt a, 1/root a 5, 1/(a*root a 3)
    , a^2/(a*sqrt a), 1/(a^3*sqrt a), a^3/(a^2/root a 3)
    ]
  ]
 where
   a = Var "a"
   b = Var "b"
   
brokenExp2 :: [[Expr]]
brokenExp2 =
   [ [ sqrt (1/a^2), root (1/a^5) 3, sqrt (1/a^5), root (1/a^3) 5
     , sqrt (a^6), root (a^6) 3, sqrt (a^4), root (a^9) 3
     ]
   , [ (1/a^3)/sqrt a, (1/a^4)/root (a^2) 3, sqrt a/(1/a^2)
     , root a 3/(1/a^5), (a^2*sqrt a)/(a*root a 3)
     , (a^3*sqrt a)/(a^2*root (a^2) 3), (a^2*root a 5)/(a^3*root a 3)
     , (a^4*root a 3)/(a^6*sqrt a)
     ]
   ]
 where
   a = Var "a"
   
brokenExp3 :: [[Expr]]
brokenExp3 =
   [ [root x 3, 1/root x 4, sqrt (1/x), x^2/root (x^2) 5]
   , [sqrt x/x^2, root (x/x^3) 3, x*root x 3, root x 3*root (1/x^2) 4]
   ]
 where
   x = Var "x"