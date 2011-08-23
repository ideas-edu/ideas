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
-- Example exercises from the Digital Mathematics Environment (DWO)
--
-----------------------------------------------------------------------------
module Domain.Math.Equation.Examples
   ( fillInResult, coverUpEquations
   ) where

import Common.Rewriting
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Prelude hiding ((^))

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