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
      [ calcPlusWith     "nat" natView
      , calcMinusWith    "nat" natView
      , calcTimesWith    "nat" natView
      , calcDivisionWith "nat" natView
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
      [ calcPlusWith     "int" integerNormalForm
      , calcMinusWith    "int" integerNormalForm
      , calcTimesWith    "int" integerNormalForm
      , calcDivisionWith "int" integerNormalForm
      , doubleNegate, negateZero
      ]

rationalStrategy :: LabeledStrategy (Context Expr)
rationalStrategy = label "simplify" $ 
   repeat $ somewhere $ alternatives $ map use
      [ calcPlusWith     "rational" rationalRelaxedForm
      , calcMinusWith    "rational" rationalRelaxedForm
      , calcTimesWith    "rational" rationalRelaxedForm
      , calcDivisionWith "int"      integerNormalForm
      , doubleNegate, negateZero, divisionDenominator
      , divisionNumerator, simplerFraction
      ]

fractionStrategy :: LabeledStrategy (Context Expr)
fractionStrategy = label "simplify" $ 
   repeat $ 
      somewhere 
         (  use (calcPlusWith     "int" integerNormalForm)
        <|> use (calcMinusWith    "int" integerNormalForm)
        <|> use (calcTimesWith    "int" integerNormalForm) -- not needed?
        -- <|> use (calcDivisionWith "int" integerNormalForm)  -- not needed?
         ) |> 
      somewhere
         (  use doubleNegate <|> use negateZero <|> use divisionDenominator  
        <|> use fractionPlus <|> use fractionTimes <|> use divisionNumerator
         ) |>
      somewhere (use fractionPlusScale) |>
      somewhere (use simplerFraction)