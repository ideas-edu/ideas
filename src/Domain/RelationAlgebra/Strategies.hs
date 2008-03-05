module Domain.RelationAlgebra.Strategies where

import Domain.RelationAlgebra.Rules
import Domain.RelationAlgebra.Formula
import Common.Context
import Common.Strategy
import Prelude hiding (repeat)

toCNF :: LabeledStrategy (Context RelAlg)
toCNF = label "To CNF" $ 
 repeat ( topDown ( alternatives ( map liftRuleToContext (
 				 [ruleRemCompl
                                 , ruleRemRedunExprs 
                                 , ruleDoubleNegation
				 , ruleIdemp 
                                 , ruleAbsorp
				 , ruleAbsorpCompl
                                 ] ++
                                  invRules))
				 
		  )|> topDown (alternatives (map liftRuleToContext (
		  		            [ruleCompOverUnion
		                            , ruleAddOverIntersec
					    , ruleDeMorgan
					    , ruleNotOverComp
					    , ruleNotOverAdd
					    ]))
		               ) 
		   |> somewhere (liftRuleToContext ruleUnionOverIntersec)
	)
       		  
   


   
   
