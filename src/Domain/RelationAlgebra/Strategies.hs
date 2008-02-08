module Domain.RelationAlgebra.Strategies where

import Domain.RelationAlgebra.Rules
import Domain.RelationAlgebra.Formula
import Domain.RelationAlgebra.Zipper



import Common.Strategy
import Prelude hiding (repeat)
toCNF :: LabeledStrategy RelAlgInContext
toCNF = label "To CNF" $ 
 repeat ( topDown ( alternatives ( map liftRelAlgRule (
 				 [ruleRemCompl
                                 , ruleRemRedunExprs 
                                 , ruleDoubleNegation
				 , ruleIdemp 
                                 , ruleAbsorp
				 , ruleAbsorpCompl
                                 ] ++
                                  invRules))
				 
		  )|> topDown (alternatives (map liftRelAlgRule (
		  		            [ruleCompOverUnion
		                            , ruleAddOverIntersec
					    , ruleDeMorgan
					    , ruleNotOverComp
					    , ruleNotOverAdd
					    ]))
		               ) 
		   |> somewhere (liftRelAlgRule ruleUnionOverIntersec)
	)
       		  
   


   
   
