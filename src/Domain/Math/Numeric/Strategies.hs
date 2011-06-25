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
-----------------------------------------------------------------------------
module Domain.Math.Numeric.Strategies
   ( naturalStrategy, integerStrategy
   , rationalStrategy, fractionStrategy
   ) where

import Common.Context
import Common.Strategy
import Common.View
import Domain.Math.Expr
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Views
import Prelude hiding (repeat)

------------------------------------------------------------
-- Strategies

naturalStrategy :: LabeledStrategy (Context Expr)
naturalStrategy = label "simplify" $ 
   repeat $ somewhere $ alternatives $ map use
      [ calcPlusWith     "natural" natView
      , calcMinusWith    "natural" natView
      , calcTimesWith    "natural" natView
      , calcDivisionWith "natural" natView
      , doubleNegate, negateZero, plusNegateLeft, plusNegateRight
      , minusNegateLeft, minusNegateRight, timesNegateLeft
      , timesNegateRight, divisionNegateLeft, divisionNegateRight  
      ]
 where
   natView = makeView f fromInteger
    where
      f (Nat n) = Just n
      f _       = Nothing

integerStrategy :: LabeledStrategy (Context Expr)
integerStrategy = label "simplify" $ 
   repeat $ somewhere $ alternatives $ map use
      [ calcPlusWith     "integer" integerNF
      , calcMinusWith    "integer" integerNF
      , calcTimesWith    "integer" integerNF
      , calcDivisionWith "integer" integerNF
      , doubleNegate, negateZero
      ]

rationalStrategy :: LabeledStrategy (Context Expr)
rationalStrategy = label "simplify" $ 
   repeat $ somewhere $ alternatives $ map use
      [ calcPlusWith     "rational" rationalRelaxedForm
      , calcMinusWith    "rational" rationalRelaxedForm
      , calcTimesWith    "rational" rationalRelaxedForm
      , calcDivisionWith "integer"  integerNF
      , doubleNegate, negateZero, divisionDenominator
      , divisionNumerator, simplerFraction
      ]

fractionStrategy :: LabeledStrategy (Context Expr)
fractionStrategy = label "simplify" $ 
   repeat $ 
      somewhere 
         (  use (calcPlusWith     "integer" integerNF)
        <|> use (calcMinusWith    "integer" integerNF)
        <|> use (calcTimesWith    "integer" integerNF) -- not needed?
        -- <|> use (calcDivisionWith "integer" integerNF)  -- not needed?
         ) |> 
      somewhere
         (  use doubleNegate <|> use negateZero <|> use divisionDenominator  
        <|> use fractionPlus <|> use fractionTimes <|> use divisionNumerator
         ) |>
      somewhere (use fractionPlusScale) |>
      somewhere (use simplerFraction)