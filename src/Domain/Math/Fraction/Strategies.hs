module Domain.Math.Fraction.Strategies
    (expandAndAdd) where

import Common.Library
import Domain.Math.Expr
import Domain.Math.Fraction.Rules

expandAndAdd :: LabeledStrategy (Context Expr)
expandAndAdd = label "expandAndAdd" $ 
               repeatS $ 
                   somewhere 
                       (   findLCM
                      <|>  expandToLCM 
                      <|>  use addLikeFractions
                       )

