module Domain.RelationAlgebra.Strategies where

import Common.Strategy

-- wat doet deze strategie eigenlijk? betere naam verzinnen?
toDNF =
   whenever (repeat topdown ruleRemCompl <*> repeat topdown ruleRedunExpr <*> repeat topdown ruleDoubleNot <*> repeat topdown ruleIdemp <*> repeat topdown ruleAbsorp <*> repeat topdown ruleAbsorpCompl)
   (repeat topdown rulesInvOver_ 
   <*> repeat topdown (ruleCompOverUnion <|> ruleAddOverIntersec<|> ruleDeMorgan) <|> ruleNotOverComp <|> ruleNotOverAnd
   <*> repeat somewhere ruleUnionOverIntersection
