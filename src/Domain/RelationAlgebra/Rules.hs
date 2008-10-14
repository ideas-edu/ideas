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
import Domain.RelationAlgebra.Generator()
import Common.Transformation
import Common.Rewriting

invRules :: [Rule RelAlg]
invRules = [ ruleInvOverUnion, ruleInvOverIntersec, ruleInvOverComp
           , ruleInvOverAdd, ruleInvOverNot, ruleDoubleInv
           ]
compAddRules :: [Rule RelAlg]
compAddRules = [ ruleCompOverUnion {- , ruleCompOverIntersec  -}
               , {- ruleAddOverUnion,-} ruleAddOverIntersec 
               ]
relAlgRules :: [Rule RelAlg]
relAlgRules = invRules ++ compAddRules ++ 
              [ ruleUnionOverIntersec,ruleDeMorgan, ruleIdemp
              , ruleRemCompl, ruleDoubleNegation, ruleAbsorpCompl
              , ruleAbsorp, ruleRemRedunExprs, ruleNotOverComp
              , ruleNotOverAdd
              ]
              
buggyRelAlgRules ::[Rule RelAlg]
buggyRelAlgRules = [buggyRuleIdemComp, buggyRuleIdemAdd, buggyRuleDeMorgan
                   , buggyRuleNotOverAdd, buggyRuleNotOverComp, buggyRuleParenth
                   , buggyRuleAssoc, buggyRuleInvOverComp, buggyRuleInvOverAdd
                   , buggyRuleCompOverIntersec, buggyRuleAddOverUnion, buggyRuleRemCompl
                   ]
                   
-- | 1. Alle ~ operatoren naar binnen verplaatsen

ruleInvOverUnion :: Rule RelAlg
ruleInvOverUnion = rule "InvOverUnion" $ 
   \r s -> (Inv (r :||: s)) :~> (Inv r :||: Inv s)

ruleInvOverIntersec :: Rule RelAlg
ruleInvOverIntersec = rule "InvOverIntersect" $  
   \r s -> (Inv (r :&&: s)) :~> (Inv r :&&: Inv s) --- !!!!!!! ALLEEN VOOR FUNCTIES

ruleInvOverComp :: Rule RelAlg
ruleInvOverComp = rule "InvOverComp" $ 
   \r s -> (Inv (r :.: s)) :~> (Inv s :.: Inv r)

ruleInvOverAdd :: Rule RelAlg
ruleInvOverAdd = rule "InvOverAdd" $ 
   \r s -> (Inv (r :+: s)) :~> (Inv s :+: Inv r)

ruleInvOverNot :: Rule RelAlg
ruleInvOverNot = rule "InvOverNot" $ 
   \r -> (Inv (Not r))     :~> (Not (Inv r))
   
ruleDoubleInv :: Rule RelAlg
ruleDoubleInv = rule "DoubleInv" $ 
   \r -> (Inv (Inv r))     :~> r
      



-- | 2. Alle ; en + operatoren zoveel mogelijk naar binnen verplaatsen 

ruleCompOverUnion :: Rule RelAlg
ruleCompOverUnion = ruleList "CompOverUnion" 
   [ \q r s -> (q :.: (r :||: s)) :~>  ((q :.: r) :||: (q :.: s)) 
   , \q r s -> ((q :||: r) :.: s) :~>  ((q :.: s) :||: (r :.: s)) 
   ]

ruleCompOverIntersec :: Rule RelAlg
ruleCompOverIntersec = ruleList "CompOverIntersec" 
   [ \q r s -> (q :.: (r :&&: s)) :~>  ((q :.: r) :&&: (q :.: s))  --alleen toegestaan als q een functie is!
   , \q r s -> ((q :&&: r) :.: s) :~>  ((q :.: s) :&&: (r :.: s))  --idem
   ]
ruleAddOverUnion :: Rule RelAlg
ruleAddOverUnion = ruleList "AddOverUnion"  
   [ \q r s -> (q :+: (r :||: s)) :~>  ((q :+: r) :||: (q :+: s)) --alleen toegestaan als q een functie is!
   , \q r s -> ((q :||: r) :+: s) :~>  ((q :+: s) :||: (r :+: s)) --idem
   ]

ruleAddOverIntersec :: Rule RelAlg
ruleAddOverIntersec = ruleList "AddOverIntersec"  
   [ \q r s -> (q :+: (r :&&: s)) :~>  ((q :+: r) :&&: (q :+: s))  
   , \q r s -> ((q :&&: r) :+: s) :~>  ((q :+: s) :&&: (r :+: s))  
   ]
-- | 3. Distribute union over intersection
 
ruleUnionOverIntersec :: Rule RelAlg
ruleUnionOverIntersec = ruleList "UnionOverIntersec" 
   [  \q r s -> (q :||: (r :&&: s)) :~>  ((q :||: r) :&&: (q :||: s)) 
   , \q r s -> ((q :&&: r) :||: s) :~>  ((q :||: s) :&&: (r :||: s)) 
   ]

-- | 4. De Morgan rules

ruleDeMorgan :: Rule RelAlg
ruleDeMorgan = ruleList "DeMorgan" 
   [ \r s -> Not (r :||: s) :~>  (Not r :&&: Not s)
   , \r s -> Not (r :&&: s) :~>  (Not r :||: Not s)
   ]



 
-- | 5. Idempotention

ruleIdemp :: Rule RelAlg
ruleIdemp = ruleList "Idempotency"  
   [ \r -> (r :||: r) :~>  r
   , \r -> (r :&&: r) :~>  r
   ]

-- | 6. Complement

ruleRemCompl :: Rule RelAlg
ruleRemCompl = ruleList "RemCompl" 
   [ \r -> (r :||: (Not r)) :~>  V
   , \r -> ((Not r) :||: r) :~>  V
   , \r -> (r :&&: (Not r)) :~>  E
   , \r -> ((Not r) :&&: r) :~>  E
   ]
   
-- |6a. Double negation   
ruleDoubleNegation :: Rule RelAlg
ruleDoubleNegation = rule "DoubleNegation" $ 
   \r -> (Not (Not r)) :~> r
   
-- | 7. Absorption complement

ruleAbsorpCompl :: Rule RelAlg
ruleAbsorpCompl = ruleList "AbsorpCompl" 
   [ \r s -> (r :&&: ((Not r) :||: s)) :~> (r :&&: s)  
   , \r s -> (r :&&: (s :||: (Not r))) :~> (r :&&: s)  
   , \r s -> (((Not r) :||: s) :&&: r) :~> (r :&&: s)
   , \r s -> ((s :||: (Not r)) :&&: r) :~> (r :&&: s)
   , \r s -> (r :||: ((Not r) :&&: s)) :~> (r :||: s)  
   , \r s -> (r :||: (s :&&: (Not r))) :~> (r :||: s)
   , \r s -> (((Not r) :&&: s) :||: r) :~> (r :||: s)
   , \r s -> ((s :&&: (Not r)) :||: r) :~> (r :||: s)
   ]
ruleAbsorp :: Rule RelAlg
ruleAbsorp = ruleList "Absorp"  
   [ \r s -> (r :&&: (r :||: s))       :~> r
   , \r s -> (r :&&: (s :||: r))       :~> r
   , \r s -> ((r :||: s) :&&: r)       :~> r
   , \r s -> ((s :||: r) :&&: r)       :~> r
   , \r s -> (r  :||: (r :&&: s))      :~> r
   , \r s -> (r  :||: (s :&&: r))      :~> r
   , \r s -> ((r :&&: s) :||: r)       :~> r
   , \r s -> ((s :&&: r) :||: r)       :~> r
   ]

-- | 8. Remove redundant expressions
ruleRemRedunExprs :: Rule RelAlg
ruleRemRedunExprs = ruleList "RemRedunExprs"  
   [ \r -> (r :||: V) :~> V
   , \r -> (V :||: r) :~> V 
   , \r -> (r :||: E) :~> r
   , \r -> (E :||: r) :~> r 
   , \r -> (r :&&: V) :~> r
   , \r -> (V :&&: r) :~> r 
   , \r -> (r :&&: E) :~> E
   , \r -> (E :&&: r) :~> E 
--   , (r :.: U)  :~> r
--   , (U :.: r)  :~> r
   , \_ -> (V :.: V)  :~> V
   , \r -> (r :.: E)  :~> E
   , \r -> (E :.: r)  :~> E 
   , \r -> (r :+: V)  :~> V
   , \r -> (V :+: r)  :~> V
   , \_ -> (E :+: E)  :~> E
--   , (r :+: E)  :~> r
--   , (E :+: r)  :~> r 
   , \_ -> (Not V)    :~> E
   , \_ -> (Not E)    :~> V
   , \_ -> (Inv V)    :~> V
   , \_ -> (Inv E)    :~> E
   ]
   
-- | 9. Distribute Not over . and +

ruleNotOverComp :: Rule RelAlg
ruleNotOverComp = rule "NotOverComp" $ 
    \r s -> Not (r :.: s) :~>  (Not r :+: Not s)
   
ruleNotOverAdd :: Rule RelAlg
ruleNotOverAdd = rule "NotOverAdd" $ 
   \r s -> Not (r :+: s) :~>  (Not r :.: Not s)
   
   -- Buggy rules:


    
buggyRuleIdemComp :: Rule RelAlg
buggyRuleIdemComp = buggyRule $ rule "IdemComp" $ 
   \q -> (q :.: q)  :~>  q 
    
buggyRuleIdemAdd :: Rule RelAlg
buggyRuleIdemAdd = buggyRule $ rule "IdemAdd"  $  
   \q -> (q :+: q)  :~>  q 
    


buggyRuleDeMorgan :: Rule RelAlg
buggyRuleDeMorgan = buggyRule $ ruleList "BuggyDeMorgan" 
    [ \q r -> (Not (q :&&: r)) :~>  (Not q :||: r)
    , \q r -> (Not (q :&&: r)) :~>  (q :||: Not r)
    , \q r -> (Not (q :&&: r)) :~> (Not (Not q :||: Not r))
    , \q r -> (Not (q :||: r)) :~>  (Not q :&&: r)
    , \q r -> (Not (q :||: r)) :~>  (q :&&: Not r)
    , \q r -> (Not (q :||: r)) :~> (Not (Not q :&&: Not r)) --note the firstNot in both formulas!  
    ]
    
buggyRuleNotOverAdd :: Rule RelAlg
buggyRuleNotOverAdd = buggyRule $ ruleList "BuggyNotOverAdd" 
     [ \q r -> (Not(q :+: r)) :~> (Not q :+: Not r)
     , \q r -> (Not(q :+: r)) :~> (Not q :.: r)
     , \q r -> (Not(q :+: r)) :~> (Not q :+: r)
     , \q r -> (Not(q :+: r)) :~> Not (Not q :.: Not r) --note the firstNot in both formulas! 
     ]
     
buggyRuleNotOverComp :: Rule RelAlg
buggyRuleNotOverComp = buggyRule $ ruleList "BuggyNotOverComp" 
     [ \q r -> (Not(q :.: r)) :~> (Not q :.: Not r)
     , \q r -> (Not(q :.: r)) :~> (Not q :.: r)
     , \q r -> (Not(q :.: r)) :~> (Not q :+: r)
     , \q r -> (Not(q :.: r)) :~> Not (Not q :.: Not r) --note the firstNot in both formulas! 
     ]
     
buggyRuleParenth :: Rule RelAlg
buggyRuleParenth = buggyRule $ ruleList "BuggyParenth" 
    [ \q r -> (Not (q :&&: r)) :~>  (Not q :&&: r)
    , \q r -> (Not (q :||: r)) :~>  (Not q :||: r)
    , \q r -> (Not(Not q :&&: r)) :~> (q :&&: r) 
    , \q r -> (Not(Not q :||: r)) :~> (q :||: r)
    , \q r -> (Not(Not q :.: r)) :~> (q :.: r)
    , \q r -> (Not(Not q :+: r)) :~> (q :+: r)
    , \q r -> (Inv (q :&&: r)) :~>  (Inv q :&&: r)
    , \q r -> (Inv (q :||: r)) :~>  (Inv q :||: r)
    , \q r -> (Inv(Inv q :&&: r)) :~> (q :&&: r) 
    , \q r -> (Inv(Inv q :||: r)) :~> (q :||: r)
    , \q r -> (Inv(Inv q :.: r)) :~> (q :.: r)
    , \q r -> (Inv(Inv q :+: r)) :~> (q :+: r)
    ]
    
buggyRuleAssoc :: Rule RelAlg
buggyRuleAssoc = buggyRule $ ruleList "BuggyAssoc"  
    [ \q r s -> (q :||: (r :&&: s)) :~> ((q :||: r) :&&: s)
    , \q r s -> ((q :||: r) :&&: s) :~> (q :||: (r :&&: s))
    , \q r s -> ((q :&&: r) :||: s) :~> (q :&&: (r :||: s))
    , \q r s -> (q :&&: (r :||: s)) :~> ((q :&&: r) :||: s)
    , \q r s -> (q :.: (r :||: s)) :~>  ((q :.: r) :||: s) 
    , \q r s -> ((q :||: r) :.: s) :~>  (q :||: (r :.: s)) 
    , \q r s -> (q :.: (r :&&: s)) :~>  ((q :.: r) :&&: s) 
    , \q r s -> ((q :&&: r) :.: s) :~>  (q :&&: (r :.: s)) 
    , \q r s -> (q :+: (r :||: s)) :~>  ((q :+: r) :||: s) 
    , \q r s -> ((q :||: r) :+: s) :~>  (q :||: (r :+: s)) 
    , \q r s -> (q :+: (r :&&: s)) :~>  ((q :+: r) :&&: s)  
    , \q r s -> ((q :&&: r) :+: s) :~>  (q :&&: (r :+: s))  
    ]
    

buggyRuleInvOverComp :: Rule RelAlg
buggyRuleInvOverComp = buggyRule $ rule "BuggyInvOverComp" $ 
   \r s -> (Inv (r :.: s)) :~> (Inv r :.: Inv s)

buggyRuleInvOverAdd :: Rule RelAlg
buggyRuleInvOverAdd = buggyRule $ rule "BuggyInvOverAdd" $ 
   \r s -> (Inv (r :+: s)) :~> (Inv r :+: Inv s)
   
buggyRuleCompOverIntersec :: Rule RelAlg
buggyRuleCompOverIntersec = buggyRule $ ruleList "BuggyCompOverIntersec" 
   [ \q r s -> (q :.: (r :&&: s)) :~>  ((q :.: r) :&&: (q :.: s))  --alleen toegestaan als q een functie is!
   , \q r s -> ((q :&&: r) :.: s) :~>  ((q :.: s) :&&: (r :.: s))  --idem
   ]
buggyRuleAddOverUnion :: Rule RelAlg
buggyRuleAddOverUnion = buggyRule $ ruleList "BuggyAddOverUnion" 
   [ \q r s -> (q :+: (r :||: s)) :~>  ((q :+: r) :||: (q :+: s)) --alleen toegestaan als q een functie is!
   , \q r s -> ((q :||: r) :+: s) :~>  ((q :+: s) :||: (r :+: s)) --idem
   ]
   
buggyRuleRemCompl :: Rule RelAlg
buggyRuleRemCompl = buggyRule $ ruleList "BuggyRemCompl" 
   [ \r -> (r :||: (Not r)) :~>  E
   , \r -> ((Not r) :||: r) :~>  E
   , \r -> (r :&&: (Not r)) :~>  V
   , \r -> ((Not r) :&&: r) :~>  V
   ]


    