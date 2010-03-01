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
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols

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
      , (2*a^3)^4 - 6*a^3 * ((-a)^3)^3
      , (-2*a^3)^2 - 6*(3*a)^2 * (-4)*a^4
      ]
