module Domain.RelationAlgebra.Strategies where

import Domain.RelationaAlgebra.Formula
import Common.Strategy

toCNF :: Strategy RelAlg
toCNF =
   whenever (repeat topdown ruleRemCompl <*> repeat topdown ruleRedunExpr <*> repeat topdown ruleDoubleNot <*> repeat topdown ruleIdemp <*> repeat topdown ruleAbsorp <*> repeat topdown ruleAbsorpCompl)
   (repeat topdown rulesInvOver_ 
   <*> repeat topdown (ruleCompOverUnion <|> ruleAddOverIntersec<|> ruleDeMorgan) <|> ruleNotOverComp <|> ruleNotOverAnd
   <*> repeat somewhere ruleUnionOverIntersection
