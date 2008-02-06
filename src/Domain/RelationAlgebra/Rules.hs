module Domain.RelationAlgebra.Rules where

-- local logic variables
q, r, s :: Logic
q:r:s:_ = map makeVarInt [0..]

-- | 1. Alle ~ operatoren naar binnen verplaatsen

ruleInvOverUnion :: RelAlgRule
ruleInvOverUnion = makeRule "InvOverUnion" $
   (Inv (r :||: s)) |- (Inv r :||: Inv s)

ruleInvOverIntersec :: RelAlgRule
ruleInvOverIntersec = makeRule "InvOverIntersect" $
   (Inv (r :&&: s)) |- (Inv r :&&: Inv s)

ruleInvOverComp :: RelAlgRule
ruleInvOverComp = makeRule "InvOverComp" $
   (Inv (r :;;: s)) |- (Inv s :;;: Inv r)

ruleInvOverAdd :: RelAlgRule
ruleInvOverAdd = makeRule "InvOverAdd" $
   (Inv (r :tt: s)) |- (Inv s :tt: Inv r)

ruleInvOverNot :: RelAlgRule
ruleInvOverUnion = makeRule "InvOverNot" $
   (Inv (Not r)     |- (Not (Inv r))



-- | 2. Alle ; en tt operatoren zoveel mogelijk naar binnen verplaatsen 

ruleCompOverUnion :: RelAlgRule
ruleCompOverUnion = makeRuleList "CompOverUnion" 
   [ (q :;;: (r :||: s) |-  ((q :;;: r) :||: (q :;;: s)) 
   , (q :||: r) :;;: s) |-  ((q :;;: s) :||: (r :;;: s)) 
   ]

ruleCompOverIntersec :: RelAlgRule
ruleCompOverIntersec = makeRuleList "CompOverIntersec" 
   [ (q :;;: (r :&&: s) |-  ((q :;;: r) :&&: (q :;;: s))  --alleen toegestaan als q een functie is!
   , (q :&&: r) :;;: s) |-  ((q :;;: s) :&&: (r :;;: s))  --idem
   ]
ruleAddOverUnion :: RelAlgRule
ruleAddOverUnion = makeRuleList "AddOverUnion" 
   [ (q :tt: (r :||: s) |-  ((q :tt: r) :||: (q :tt: s)) --alleen toegestaan als q een functie is!
   , (q :||: r) :tt: s) |-  ((q :tt: s) :||: (r :tt: s)) --idem
   ]

ruleAddOverIntersec :: RelAlgRule
ruleAddOverIntersec = makeRuleList "AddOverIntersec" 
   [ (q :tt: (r :&&: s) |-  ((q :tt: r) :&&: (q :tt: s))  
   , (q :&&: r) :tt: s) |-  ((q :tt: s) :&&: (r :tt: s))  
   ]
-- | 3. Distribute union over intersection

ruleUnionOverIntersec :: RelAlgRule
ruleUnionOverIntersec = makeRuleList "UnionOverIntersec 
   [ (q :||: (r :&&: s) |-  ((q :||: r) :&&: (q :||: s)) 
   , (q :&&: r) :||: s) |-  ((q :||: s) :&&: (r :||: s)) 
   ]

--| 4. De Morgan rules

ruleDeMorgan :: RelAlgRule
ruleDeMorgan = makeRuleList "DeMorgan" 
   [ Not (r :||: s) |-  (Not r :&&: Not s)
   , Not (r :&&: s) |-  (Not r :||: Not s)
   ]



   ]
--| 5. Idempotention

ruleIdemp :: RelAlgRule
ruleIdemp = makeRuleList "Idempotency" 
   [ (r :||: r) |-  r
   , (r :&&: r) |-  r
   ]

--| 6. Complement

ruleRemCompl :: RelAlgRule
ruleRemCompl = makeRuleList "RemCompl" 
   [ (r :||: (Not r)) |-  U
   , ((Not r) :||: r) |-  U
   , (r :&&: (Not r)) |-  E
   , ((Not r) :&&: r) |-  E
   ]

--| 7. Absorption complement

ruleAbsorpCompl :: RelAlgRule
ruleAbsorbCompl = makeRuleList "AbsorbCompl"
   [ (r :&&: ((Not r) :||: s)) |- (r :&&: s)  
   , (r :&&: (s :||: (Not r))) |- (r :&&: s)  
   , (((Not r) :||: s) :&&: r) |- (r :&&: s)
   , ((s :||: (Not r)) :&&: r) |- (r :&&: s)
   , (r :||: ((Not r) :&&: s)) |- (r :||: s)  
   , (r :||: (s :&&: (Not r))) |- (r :||: s)
   , (((Not r) :&&: s) :||: r) |- (r :||: s)
   , ((s :&&: (Not r)) :||: r) |- (r :||: s)
   
   , (r :&&: (r :||: s))       |- r
   , (r :&&: (s :||: r))       |- r
   , ((r :||: s) :&&: r)       |- r
   , ((s :||: r) :&&: r)       |- r
   , (r  :||: (r :&&: s))      |- r
   , (r  :||: (s :&&: r))      |- r
   , ((r :&&: s) :||: r)       |- r
   , ((s :&&: r) :||: r)       |- r
   ]

--| 8. Remove redundant expressions
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
   
--| 9. Distribute Not over ;; and tt

ruleNotOverComp :: RelAlgRule
ruleNotOverComp = makeRule "NotOverComp" $
    Not (r :;;: s) |-  (Not r :;;: Not s)
   
ruleNotOverAdd :: RelAlgRule
ruleNotOverAdd = makeRule "NotOverAdd" $
   Not (r :tt: s) |-  (Not r :tt: Not s)



 