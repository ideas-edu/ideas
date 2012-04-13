-----------------------------------------------------------------------------
-- Copyright 2012, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  johan.jeuring@ou.nl
-- Stability   :  alpha
-- Portability :  portable (depends on ghc)
--
-- Example exercises from ActiveMath
--
-----------------------------------------------------------------------------
module Domain.Math.Numeric.Examples
   ( fractionExamples
   , fractionLiberalExamples
   ) where

import Common.Exercise
import Domain.Math.Expr
import Prelude hiding ((^))

fractionExamples :: Examples Expr
fractionExamples =
   level Easy 
      [ 3/5 + 1/3
      , 1/2 + 2/4
      , 3/2 + 2/3
      , 7/11 + 3/11 ] ++
   level Medium -- NKBW tests (bridging test between secondary and tertiary education)
      [ (2/3+2/7)/(4/7-1/3) -- VWO B, Sep 2010
      , 18/(4/5-1/2) -- VWO A, Sep 2010
      ]

fractionLiberalExamples :: Examples Expr
fractionLiberalExamples =
   level Easy 
      [ 3/5 + 1/3
      , 1/2 + 2/4
      , 3/2 + 2/3
      , 7/11 + 3/11 ]
