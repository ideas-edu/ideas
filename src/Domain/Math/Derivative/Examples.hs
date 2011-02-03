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
-- Example exercises from the Digital Mathematics Environment (DWO),
-- see: http://www.fi.uu.nl/dwo/gr/frameset.html.
--
-----------------------------------------------------------------------------
module Domain.Math.Derivative.Examples
   ( diffSet1, diffSet2, diffSet3, diffSet4
   , diffSet5, diffSet6, diffSet7, diffSet8
   ) where

import Domain.Math.Expr
import Prelude hiding ((^))
import Data.Maybe

differentiateLists :: [[Expr]] -> [[Expr]]
differentiateLists = map (map differentiate)

differentiate :: Expr -> Expr
differentiate a = 
   let x = fromMaybe "x" (selectVar a) 
   in unary diffSymbol $ binary lambdaSymbol (Var x) a

----------------------------------------------------------
-- HAVO B applets

-- Hoofdstuk 6, differentieer
-- Bereken de afgeleide
diffSet1 :: [[Expr]]
diffSet1 = differentiateLists $
   let x = Var "x" in
   let p = Var "p" in
   let q = Var "q" in
   let r = Var "r" in
   [ [ 3*x^4 - 7*x^2, -x^3-5*x, 1/2*x^6-5*x^2+4, -1/3*x^3+(1+1/2)*x^2-x+1]
   , [ -x^5+5*x+23, -2*p^4+5*p-12, 3/5*q^5-q^3+4*q, -2/3*r^6+1/4*r^4-3*r+7]  
   , -- werk eerst de haakjes weg
     [ (x-2)^2, -(1-3*x)^2, (x-1)*(2*x+5), -(1-3*x)*(2*x+7)]
     -- differentieer
   , [x^3-x*(x+5), -2*(p+1)*(p-12), q*(q^5-q^3)+3*q^2+4, -3*r*(r-1)*(r+2)]
   ]
   
----------------------------------------------------------
-- VWO A/C applets

-- Hoofdstuk 7, differentieer
diffSet2 :: [[Expr]]
diffSet2 = differentiateLists $
   let x = Var "x" in
   [ [ 5*x^2, -4*x^2, 10*x^2-8, -8*x^2+7]
   , [ 3*x^2+4*x, -0.5*x^2-2*x, -8*x^2+7*x-3, -0.25*x^2+x-1]
   , [ (x+2)^2, (5*x+7)*(4-3*x), (3*x+6)^2-8*x
     , 5*(x-3)^2+5*x, 5*(x-3)^2+5*(2*x-1), -3*(x-1)*(5-9*x)-8*(x-7) ]
   ]
   
-- Hoofdstuk 7, bereken de afgeleide: zelfde als Havo B applet

----------------------------------------------------------
-- VWO B applets

-- Hoofdstuk 3, differentieren: zelfde als Havo B applet

-- Hoofdstuk 7
-- Gebruik de productregel
diffSet3 :: [[Expr]]
diffSet3 = differentiateLists $
   let x = Var "x" in
   [ [ (x^2+2*x)*(3*x+5), (2*x^2-3*x)*(4*x+1), (3*x^3+4*x)*(x^2-2)
     , (4*x^3-x)*(3*x^2+7*x), (x^2+2*x)*(x^3-4*x^2+3), (5*x-7)*(2*x^3-3*x+1)
     , (3*x^2+2)*(5*x^3+4*x^2-7*x), (4*x+1)*(3*x^3-x^2+2*x)
     ]
   , [ (3*x+1)^2, (5*x-2)^2, (2*x+7)^2, (4*x-3)^2
     , (2*x^2-3*x)^2, (3*x^2+2)^2, 2*x^3-3*x^2, (5*x^3+7*x)^2
     ]
   ]
   
-- Gebruik de quotientregel
diffSet4 :: [[Expr]]
diffSet4 = differentiateLists $
   let x = Var "x" in
   [ [ 5/(x-1), 3/(x+2), (-2)/(x-3), (-3)/(x+4), 3/(2*x-1)
     , 2/(3*x+4), (-4)/(3*x-1), (-2)/(4*x+3) 
     ]
   , [ (x+1)/(x-2), (x-3)/(x+4), (x+5)/(x-1), (x-2)/(x+1)
     , (2*x+3)/(4*x-1), (3*x-1)/(2*x+1), (4*x+3)/(3*x-2), (5*x-2)/(3*x+4)
     ]
   , [ (3*x^2)/(2*x^3+4), (2*x^3)/(3*x^2-1), (x^2)/(4*x^3-2)
     , (3*x^3)/(5*x^2+7), (1-x^3)/(x+4), (x+3)/(2-x^2)
     , (1-2*x^3)/(x+1), (x+5)/(2-3*x^2)
     ]
   , [ (2-x)/(x^2+1)+2*x^3, (x^3-3)/(4-x)+x^2
     , (3-2*x)/(2*x^2-3)+x^3, (2*x^3-4)/(6-5*x)+4*x^2
     ] 
   ]
   
-- differentieer x^n (n geheel), noteer zonder negatieve exponent
diffSet5 :: [[Expr]]
diffSet5 = differentiateLists $
   let x = Var "x" in
   [ [ 4/x^2, 5/x^3, 2/x^4, 3/x^5, 1/9*x^2, 1/7*x^3, 1/5*x^4, 1/8*x^5 ]
   , [ 3*x^2-4/(x^2), 7*x^3-2/(x^3), 2*x^4-5/(x^4), 2*x^5-6/(x^5) 
     , (3*x+2)/(x^3), (2*x^2-4)/x^5, (4*x-3)/x^2, (6*x^2+5)/x^4 
     ]
   , -- herleid de afgeleide tot 1 breuk
     [ (2*x^4+3)/x^2, (2*x^5-5)/x^3, (4*x^5-1)/x^2, (4*x^4+3)/x^3
     , (3*x-1)/(7*x^2), (2*x^3+1)/(3*x^4), (x^2-2)/(3*x^3), (x+5)/(6*x^3)
     ]
   ]
   
-- differentieer x^r (r uit R), noteer zonder negatieve en gebroken exponent
diffSet6 :: [[Expr]]
diffSet6 = differentiateLists $
   let x = Var "x" in
   [ [ x*root x 3, x^3*sqrt x, x*root x 5, x^4*sqrt x, 1/(x*root x 3)
     , 1/(x^3*sqrt x), 1/(x*root x 5), 1/(x^4*sqrt x)
     ]
   , [ x^2*root (x^2) 3, x*root (x^3) 4, x^3*root (x^2) 5, x^2*root (x^3) 5
     , (x^3+1)*(2+sqrt x), (3+x^2)*(1+root x 3), (x^2+1)*(root x 3+2)
     , (3+x^3)*(sqrt x+1) 
     ]
   , [ (sqrt x + 1)^2, (x*sqrt x-3)^2, (sqrt x-2)^2, (x*sqrt x+1)^2
     , (x+2)/sqrt x, (x-3)/sqrt x, (x-4)/sqrt x, (x+5)/sqrt x
     ]
   , [ (x-2)/(x*sqrt x), (x+3)/(x*sqrt x), (x+4)/(x*sqrt x), (x-5)/(x*sqrt x)
     , (x^2+2)/(3*sqrt x), (x^2-3)/(4*sqrt x)
     , (x^2+4)/(2*sqrt x), (x^2-6)/(3*sqrt x)
     ]
   , [ (x+3)/(x^2*sqrt x), (x-1)/(x^3*sqrt x), (x-2)/(x^2*sqrt x)
     , (x+4)/(x^3*sqrt x), (sqrt x-2)/x^2, (2*sqrt x+1)/x^2
     , (1-sqrt x)/x, (3*sqrt x+2)/x
     ]
   ]
   
-- differentieren met de kettingregel
diffSet7 :: [[Expr]]
diffSet7 = differentiateLists $
   let x = Var "x" in
   [ [ 2*(x^2+3*x)^5, 3*(x^3-4*x)^6, -6*(x^2+2*x)^4, -5*(x^3-3*x^2)^3]
   , [ -(2/(x^2+3*x)^5),-(3/(x^3-4*x)^6), 6/(x^2+2*x)^4, 5/(x^3-3*x^2)^3]
   , [ sqrt (3*x^4-x), sqrt (x^3+5*x^2), sqrt (6*x^2+x), sqrt (7*x^3-3*x^2)]
   , [ 1/sqrt (3*x-2), 1/sqrt (8*x+5), 1/sqrt (3*x-4), 1/sqrt (5*x-2)]
   , [ (2*x-1)^2*sqrt (2*x-1), (3*x^2+2)*sqrt (3*x^2+2)
     , (3*x+5)^3*sqrt (3*x+5), (4*x^3-7)*sqrt (4*x^3-7)
     ]
   ]
   
-- differentieren met de kettingregel gecombineerd
diffSet8 :: [[Expr]]
diffSet8 = differentiateLists $
   let x = Var "x" in
   [ [ 2*x*sqrt (4*x+3), 3*x*sqrt (2*x-5), 4*x*sqrt (3*x+2), 2*x*sqrt (5*x-3)]
   , [ x^2*(4*x^2-2)^3, x^3*(3*x-4)^3, x^4*(3*x^2+1)^5, x^5*(4*x+3)^4]
   , [ (x+3)/sqrt (2*x-1), (x+7)/sqrt (4*x+3)
     , (x-2)/sqrt (3*x+1), (x-7)/sqrt (5*x-4) 
     ]
   , [ sqrt (2*x^2-1)/(x+3), sqrt (4*x^2+3)/(x+7)
     , sqrt (3*x^2+1)/(x-2), sqrt (5*x^2-4)/(x-7)
     ]
   ]