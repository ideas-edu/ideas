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
import Common.Rewriting

-- local relalg variables
q, r, s :: RelAlg
q:r:s:_ = metaVars

type RelAlgRule = Rule RelAlg


invRules :: [RelAlgRule]
invRules = [ ruleInvOverUnion, ruleInvOverIntersec, ruleInvOverComp
           , ruleInvOverAdd, ruleInvOverNot, ruleDoubleInv
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
              
buggyRelAlgRules ::[RelAlgRule]
buggyRelAlgRules = [buggyRuleIdemComp, buggyRuleIdemAdd, buggyRuleDeMorgan
                   , buggyRuleNotOverAdd, buggyRuleNotOverComp, buggyRuleParenth
                   , buggyRuleAssoc, buggyRuleInvOverComp, buggyRuleInvOverAdd
                   , buggyRuleCompOverIntersec, buggyRuleAddOverUnion, buggyRuleRemCompl
                   ]
                   
-- | 1. Alle ~ operatoren naar binnen verplaatsen

ruleInvOverUnion :: RelAlgRule
ruleInvOverUnion = makeRule "InvOverUnion" $ RewriteRule $
   (Inv (r :||: s)) |- (Inv r :||: Inv s)

ruleInvOverIntersec :: RelAlgRule
ruleInvOverIntersec = makeRule "InvOverIntersect" $ RewriteRule $ 
   (Inv (r :&&: s)) |- (Inv r :&&: Inv s)

ruleInvOverComp :: RelAlgRule
ruleInvOverComp = makeRule "InvOverComp" $ RewriteRule $
   (Inv (r :.: s)) |- (Inv s :.: Inv r)

ruleInvOverAdd :: RelAlgRule
ruleInvOverAdd = makeRule "InvOverAdd" $ RewriteRule $
   (Inv (r :+: s)) |- (Inv s :+: Inv r)

ruleInvOverNot :: RelAlgRule
ruleInvOverNot = makeRule "InvOverNot" $ RewriteRule $
   (Inv (Not r))     |- (Not (Inv r))
   
ruleDoubleInv :: RelAlgRule
ruleDoubleInv = makeRule "DoubleInv" $ RewriteRule $
   (Inv (Inv r))     |- r
      



-- | 2. Alle ; en + operatoren zoveel mogelijk naar binnen verplaatsen 

ruleCompOverUnion :: RelAlgRule
ruleCompOverUnion = makeRuleList "CompOverUnion" $ map RewriteRule
   [ (q :.: (r :||: s)) |-  ((q :.: r) :||: (q :.: s)) 
   , ((q :||: r) :.: s) |-  ((q :.: s) :||: (r :.: s)) 
   ]

ruleCompOverIntersec :: RelAlgRule
ruleCompOverIntersec = makeRuleList "CompOverIntersec" $ map RewriteRule
   [ (q :.: (r :&&: s)) |-  ((q :.: r) :&&: (q :.: s))  --alleen toegestaan als q een functie is!
   , ((q :&&: r) :.: s) |-  ((q :.: s) :&&: (r :.: s))  --idem
   ]
ruleAddOverUnion :: RelAlgRule
ruleAddOverUnion = makeRuleList "AddOverUnion"  $ map RewriteRule
   [ (q :+: (r :||: s)) |-  ((q :+: r) :||: (q :+: s)) --alleen toegestaan als q een functie is!
   , ((q :||: r) :+: s) |-  ((q :+: s) :||: (r :+: s)) --idem
   ]

ruleAddOverIntersec :: RelAlgRule
ruleAddOverIntersec = makeRuleList "AddOverIntersec"  $ map RewriteRule
   [ (q :+: (r :&&: s)) |-  ((q :+: r) :&&: (q :+: s))  
   , ((q :&&: r) :+: s) |-  ((q :+: s) :&&: (r :+: s))  
   ]
-- | 3. Distribute union over intersection
 
ruleUnionOverIntersec :: RelAlgRule
ruleUnionOverIntersec = makeRuleList "UnionOverIntersec" $ map RewriteRule
   [ (q :||: (r :&&: s)) |-  ((q :||: r) :&&: (q :||: s)) 
   , ((q :&&: r) :||: s) |-  ((q :||: s) :&&: (r :||: s)) 
   ]

-- | 4. De Morgan rules

ruleDeMorgan :: RelAlgRule
ruleDeMorgan = makeRuleList "DeMorgan" $ map RewriteRule
   [ Not (r :||: s) |-  (Not r :&&: Not s)
   , Not (r :&&: s) |-  (Not r :||: Not s)
   ]



 
-- | 5. Idempotention

ruleIdemp :: RelAlgRule
ruleIdemp = makeRuleList "Idempotency" $ map RewriteRule 
   [ (r :||: r) |-  r
   , (r :&&: r) |-  r
   ]

-- | 6. Complement

ruleRemCompl :: RelAlgRule
ruleRemCompl = makeRuleList "RemCompl" $ map RewriteRule
   [ (r :||: (Not r)) |-  U
   , ((Not r) :||: r) |-  U
   , (r :&&: (Not r)) |-  E
   , ((Not r) :&&: r) |-  E
   ]
   
-- |6a. Double negation   
ruleDoubleNegation :: RelAlgRule
ruleDoubleNegation = makeRule "DoubleNegation" $ RewriteRule $
   (Not (Not r)) |- r
   
-- | 7. Absorption complement

ruleAbsorpCompl :: RelAlgRule
ruleAbsorpCompl = makeRuleList "AbsorpCompl" $ map RewriteRule
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
ruleAbsorp = makeRuleList "Absorp" $ map RewriteRule $
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
ruleRemRedunExprs = makeRuleList "RemRedunExprs" $ map RewriteRule $
   [ (r :||: U) |- U
   , (U :||: r) |- U 
   , (r :||: E) |- r
   , (E :||: r) |- r 
   , (r :&&: U) |- r
   , (U :&&: r) |- r 
   , (r :&&: E) |- E
   , (E :&&: r) |- E 
--   , (r :.: U)  |- r
--   , (U :.: r)  |- r
   , (U :.: U)  |- U
   , (r :.: E)  |- E
   , (E :.: r)  |- E 
   , (r :+: U)  |- U
   , (U :+: r)  |- U
   , (E :+: E)  |- E
--   , (r :+: E)  |- r
--   , (E :+: r)  |- r 
   , (Not U)    |- E
   , (Not E)    |- U
   , (Inv U)    |- U
   , (Inv E)    |- E
   ]
   
-- | 9. Distribute Not over . and +

ruleNotOverComp :: RelAlgRule
ruleNotOverComp = makeRule "NotOverComp" $ RewriteRule $
    Not (r :.: s) |-  (Not r :+: Not s)
   
ruleNotOverAdd :: RelAlgRule
ruleNotOverAdd = makeRule "NotOverAdd" $ RewriteRule $
   Not (r :+: s) |-  (Not r :.: Not s)
   
   -- Buggy rules:


    
buggyRuleIdemComp :: RelAlgRule
buggyRuleIdemComp = buggyRule $ makeRule "IdemComp" $ RewriteRule $
    (q :.: q)  |-  q 
    
buggyRuleIdemAdd :: RelAlgRule
buggyRuleIdemAdd = buggyRule $ makeRule "IdemAdd"  $ RewriteRule $ 
    (q :+: q)  |-  q 
    


buggyRuleDeMorgan :: RelAlgRule
buggyRuleDeMorgan = buggyRule $ makeRuleList "BuggyDeMorgan" $ map RewriteRule
    [ (Not (q :&&: r)) |-  (Not q :||: r)
    , (Not (q :&&: r)) |-  (q :||: Not r)
    , (Not (q :&&: r)) |- (Not (Not q :||: Not r))
    , (Not (q :||: r)) |-  (Not q :&&: r)
    , (Not (q :||: r)) |-  (q :&&: Not r)
    , (Not (q :||: r)) |- (Not (Not q :&&: Not r)) --note the firstNot in both formulas!  
    ]
    
buggyRuleNotOverAdd :: RelAlgRule
buggyRuleNotOverAdd = buggyRule $ makeRuleList "BuggyNotOverAdd" $ map RewriteRule
     [(Not(q :+: r)) |- (Not q :+: Not r)
     ,(Not(q :+: r)) |- (Not q :.: r)
     ,(Not(q :+: r)) |- (Not q :+: r)
     ,(Not(q :+: r)) |- Not (Not q :.: Not r) --note the firstNot in both formulas! 
     ]
     
buggyRuleNotOverComp :: RelAlgRule
buggyRuleNotOverComp = buggyRule $ makeRuleList "BuggyNotOverComp" $ map RewriteRule
     [(Not(q :.: r)) |- (Not q :.: Not r)
     ,(Not(q :.: r)) |- (Not q :.: r)
     ,(Not(q :.: r)) |- (Not q :+: r)
     ,(Not(q :.: r)) |- Not (Not q :.: Not r) --note the firstNot in both formulas! 
     ]
     
buggyRuleParenth :: RelAlgRule
buggyRuleParenth = buggyRule $ makeRuleList "BuggyParenth" $ map RewriteRule
    [ (Not (q :&&: r)) |-  (Not q :&&: r)
    , (Not (q :||: r)) |-  (Not q :||: r)
    , (Not(Not q :&&: r)) |- (q :&&: r) 
    , (Not(Not q :||: r)) |- (q :||: r)
    , (Not(Not q :.: r)) |- (q :.: r)
    , (Not(Not q :+: r)) |- (q :+: r)
    , (Inv (q :&&: r)) |-  (Inv q :&&: r)
    , (Inv (q :||: r)) |-  (Inv q :||: r)
    , (Inv(Inv q :&&: r)) |- (q :&&: r) 
    , (Inv(Inv q :||: r)) |- (q :||: r)
    , (Inv(Inv q :.: r)) |- (q :.: r)
    , (Inv(Inv q :+: r)) |- (q :+: r)
    ]
    
buggyRuleAssoc :: RelAlgRule
buggyRuleAssoc = buggyRule $ makeRuleList "BuggyAssoc" $ map RewriteRule 
    [ (q :||: (r :&&: s)) |- ((q :||: r) :&&: s)
    , ((q :||: r) :&&: s) |- (q :||: (r :&&: s))
    , ((q :&&: r) :||: s) |- (q :&&: (r :||: s))
    , (q :&&: (r :||: s)) |- ((q :&&: r) :||: s)
    , (q :.: (r :||: s)) |-  ((q :.: r) :||: s) 
    , ((q :||: r) :.: s) |-  (q :||: (r :.: s)) 
    , (q :.: (r :&&: s)) |-  ((q :.: r) :&&: s) 
    , ((q :&&: r) :.: s) |-  (q :&&: (r :.: s)) 
    , (q :+: (r :||: s)) |-  ((q :+: r) :||: s) 
    , ((q :||: r) :+: s) |-  (q :||: (r :+: s)) 
    , (q :+: (r :&&: s)) |-  ((q :+: r) :&&: s)  
    , ((q :&&: r) :+: s) |-  (q :&&: (r :+: s))  
    ]
    

buggyRuleInvOverComp :: RelAlgRule
buggyRuleInvOverComp = buggyRule $ makeRule "BuggyInvOverComp" $ RewriteRule $
   (Inv (r :.: s)) |- (Inv r :.: Inv s)

buggyRuleInvOverAdd :: RelAlgRule
buggyRuleInvOverAdd = buggyRule $ makeRule "BuggyInvOverAdd" $ RewriteRule $
   (Inv (r :+: s)) |- (Inv r :+: Inv s)
   
buggyRuleCompOverIntersec :: RelAlgRule
buggyRuleCompOverIntersec = buggyRule $ makeRuleList "BuggyCompOverIntersec" $ map RewriteRule
   [ (q :.: (r :&&: s)) |-  ((q :.: r) :&&: (q :.: s))  --alleen toegestaan als q een functie is!
   , ((q :&&: r) :.: s) |-  ((q :.: s) :&&: (r :.: s))  --idem
   ]
buggyRuleAddOverUnion :: RelAlgRule
buggyRuleAddOverUnion = buggyRule $ makeRuleList "BuggyAddOverUnion" $ map RewriteRule
   [ (q :+: (r :||: s)) |-  ((q :+: r) :||: (q :+: s)) --alleen toegestaan als q een functie is!
   , ((q :||: r) :+: s) |-  ((q :+: s) :||: (r :+: s)) --idem
   ]
   
buggyRuleRemCompl :: RelAlgRule
buggyRuleRemCompl = buggyRule $ makeRuleList "BuggyRemCompl" $ map RewriteRule
   [ (r :||: (Not r)) |-  E
   , ((Not r) :||: r) |-  E
   , (r :&&: (Not r)) |-  U
   , ((Not r) :&&: r) |-  U
   ]


    