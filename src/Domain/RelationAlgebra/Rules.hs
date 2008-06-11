-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.RelationAlgebra.Rules where

import Domain.RelationAlgebra.Formula
import Common.Transformation
import Common.Unification

-- local relalg variables
q, r, s :: RelAlg
q:r:s:_ = metaVars

type RelAlgRule = Rule RelAlg


invRules :: [RelAlgRule]
invRules = [ ruleInvOverUnion, ruleInvOverIntersec, ruleInvOverComp
           , ruleInvOverAdd, ruleInvOverNot
           ]
compAddRules :: [RelAlgRule]
compAddRules = [ ruleCompOverUnion {- , ruleCompOverIntersec  -}
	       , {- ruleAddOverUnion,-} ruleAddOverIntersec 
	       ]
relAlgRules :: [RelAlgRule]
relAlgRules = invRules ++ compAddRules ++ 
	      [ ruleUnionOverIntersec,ruleDeMorgan, ruleIdemp
	      , ruleRemCompl, ruleDoubleNegation, ruleAbsorpCompl
	      , ruleAbsorp, ruleRemRedunExprs, ruleNotOverComp
	      , ruleNotOverAdd
	      ]
	      
-- | 1. Alle ~ operatoren naar binnen verplaatsen

ruleInvOverUnion :: RelAlgRule
ruleInvOverUnion = makeRule "InvOverUnion" $
   (Inv (r :||: s)) |- (Inv r :||: Inv s)

ruleInvOverIntersec :: RelAlgRule
ruleInvOverIntersec = makeRule "InvOverIntersect" $
   (Inv (r :&&: s)) |- (Inv r :&&: Inv s)

ruleInvOverComp :: RelAlgRule
ruleInvOverComp = makeRule "InvOverComp" $
   (Inv (r :.: s)) |- (Inv s :.: Inv r)

ruleInvOverAdd :: RelAlgRule
ruleInvOverAdd = makeRule "InvOverAdd" $
   (Inv (r :+: s)) |- (Inv s :+: Inv r)

ruleInvOverNot :: RelAlgRule
ruleInvOverNot = makeRule "InvOverNot" $
   (Inv (Not r))     |- (Not (Inv r))



-- | 2. Alle ; en + operatoren zoveel mogelijk naar binnen verplaatsen 

ruleCompOverUnion :: RelAlgRule
ruleCompOverUnion = makeRuleList "CompOverUnion" 
   [ (q :.: (r :||: s)) |-  ((q :.: r) :||: (q :.: s)) 
   , ((q :||: r) :.: s) |-  ((q :.: s) :||: (r :.: s)) 
   ]

ruleCompOverIntersec :: RelAlgRule
ruleCompOverIntersec = makeRuleList "CompOverIntersec" 
   [ (q :.: (r :&&: s)) |-  ((q :.: r) :&&: (q :.: s))  --alleen toegestaan als q een functie is!
   , ((q :&&: r) :.: s) |-  ((q :.: s) :&&: (r :.: s))  --idem
   ]
ruleAddOverUnion :: RelAlgRule
ruleAddOverUnion = makeRuleList "AddOverUnion" 
   [ (q :+: (r :||: s)) |-  ((q :+: r) :||: (q :+: s)) --alleen toegestaan als q een functie is!
   , ((q :||: r) :+: s) |-  ((q :+: s) :||: (r :+: s)) --idem
   ]

ruleAddOverIntersec :: RelAlgRule
ruleAddOverIntersec = makeRuleList "AddOverIntersec" 
   [ (q :+: (r :&&: s)) |-  ((q :+: r) :&&: (q :+: s))  
   , ((q :&&: r) :+: s) |-  ((q :+: s) :&&: (r :+: s))  
   ]
-- | 3. Distribute union over intersection
 
ruleUnionOverIntersec :: RelAlgRule
ruleUnionOverIntersec = makeRuleList "UnionOverIntersec" 
   [ (q :||: (r :&&: s)) |-  ((q :||: r) :&&: (q :||: s)) 
   , ((q :&&: r) :||: s) |-  ((q :||: s) :&&: (r :||: s)) 
   ]

-- | 4. De Morgan rules

ruleDeMorgan :: RelAlgRule
ruleDeMorgan = makeRuleList "DeMorgan" 
   [ Not (r :||: s) |-  (Not r :&&: Not s)
   , Not (r :&&: s) |-  (Not r :||: Not s)
   ]



 
-- | 5. Idempotention

ruleIdemp :: RelAlgRule
ruleIdemp = makeRuleList "Idempotency" 
   [ (r :||: r) |-  r
   , (r :&&: r) |-  r
   ]

-- | 6. Complement

ruleRemCompl :: RelAlgRule
ruleRemCompl = makeRuleList "RemCompl" 
   [ (r :||: (Not r)) |-  U
   , ((Not r) :||: r) |-  U
   , (r :&&: (Not r)) |-  E
   , ((Not r) :&&: r) |-  E
   ]
   
-- |6a. Double negation   
ruleDoubleNegation :: RelAlgRule
ruleDoubleNegation = makeRule "DoubleNegation" $
   (Not (Not r)) |- r
   
-- | 7. Absorption complement

ruleAbsorpCompl :: RelAlgRule
ruleAbsorpCompl = makeRuleList "AbsorpCompl"
   [ (r :&&: ((Not r) :||: s)) |- (r :&&: s)  
   , (r :&&: (s :||: (Not r))) |- (r :&&: s)  
   , (((Not r) :||: s) :&&: r) |- (r :&&: s)
   , ((s :||: (Not r)) :&&: r) |- (r :&&: s)
   , (r :||: ((Not r) :&&: s)) |- (r :||: s)  
   , (r :||: (s :&&: (Not r))) |- (r :||: s)
   , (((Not r) :&&: s) :||: r) |- (r :||: s)
   , ((s :&&: (Not r)) :||: r) |- (r :||: s)
   ]
ruleAbsorp :: RelAlgRule
ruleAbsorp = makeRuleList "Absorp" 
   [ (r :&&: (r :||: s))       |- r
   , (r :&&: (s :||: r))       |- r
   , ((r :||: s) :&&: r)       |- r
   , ((s :||: r) :&&: r)       |- r
   , (r  :||: (r :&&: s))      |- r
   , (r  :||: (s :&&: r))      |- r
   , ((r :&&: s) :||: r)       |- r
   , ((s :&&: r) :||: r)       |- r
   ]

-- | 8. Remove redundant expressions
ruleRemRedunExprs :: RelAlgRule
ruleRemRedunExprs = makeRuleList "RemRedunExprs"
   [ (r :||: U) |- U
   , (U :||: r) |- U 
   , (r :||: E) |- r
   , (E :||: r) |- r 
   , (r :&&: U) |- r
   , (U :&&: r) |- r 
   , (r :&&: E) |- E
   , (E :&&: r) |- E 
   ]
   
-- | 9. Distribute Not over . and +

ruleNotOverComp :: RelAlgRule
ruleNotOverComp = makeRule "NotOverComp" $
    Not (r :.: s) |-  (Not r :+: Not s)
   
ruleNotOverAdd :: RelAlgRule
ruleNotOverAdd = makeRule "NotOverAdd" $
   Not (r :+: s) |-  (Not r :.: Not s)