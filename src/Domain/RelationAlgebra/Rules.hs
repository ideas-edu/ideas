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
module Domain.RelationAlgebra.Rules where

import Domain.RelationAlgebra.Formula
import Domain.RelationAlgebra.Generator()
import Common.Id
import Common.Transformation (Rule, addRuleToGroup, buggyRule)
import Common.Rewriting
import qualified Common.Transformation as Rule

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
              [ ruleUnionOverIntersec, ruleDeMorganOr, ruleDeMorganAnd, ruleIdempOr, ruleIdempAnd
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

relalg :: IsId a => a -> Id
relalg = (#) "relationalgebra"

rule :: (Builder f a, Rewrite a) => String -> f -> Rule a
rule = Rule.rule . relalg

ruleList :: (Builder f a, Rewrite a) => String -> [f] -> Rule a
ruleList = Rule.ruleList . relalg
                   
-- | 1. Alle ~ operatoren naar binnen verplaatsen

conversionGroup s = 
   addRuleToGroup (relalg "Conversion") . rule s

ruleInvOverUnion :: Rule RelAlg
ruleInvOverUnion = conversionGroup "InvOverUnion" $ 
   \r s -> Inv (r :||: s) :~> Inv r :||: Inv s

ruleInvOverIntersec :: Rule RelAlg
ruleInvOverIntersec = conversionGroup "InvOverIntersect" $  
   \r s -> Inv (r :&&: s) :~> Inv r :&&: Inv s --- !!!!!!! ALLEEN VOOR FUNCTIES

ruleInvOverComp :: Rule RelAlg
ruleInvOverComp = conversionGroup "InvOverComp" $ 
   \r s -> Inv (r :.: s) :~> Inv s :.: Inv r

ruleInvOverAdd :: Rule RelAlg
ruleInvOverAdd = conversionGroup "InvOverAdd" $ 
   \r s -> Inv (r :+: s) :~> Inv s :+: Inv r

ruleInvOverNot :: Rule RelAlg
ruleInvOverNot = conversionGroup "InvOverNot" $ 
   \r -> Inv (Not r) :~> Not (Inv r)
   
ruleDoubleInv :: Rule RelAlg
ruleDoubleInv = conversionGroup "DoubleInv" $ 
   \r -> Inv (Inv r) :~> r
      



-- | 2. Alle ; en + operatoren zoveel mogelijk naar binnen verplaatsen 

distributionGroup s = 
   addRuleToGroup (relalg "Distribution") . ruleList s

ruleCompOverUnion :: Rule RelAlg
ruleCompOverUnion = distributionGroup "CompOverUnion" 
   [ \q r s -> q :.: (r :||: s) :~>  (q :.: r) :||: (q :.: s) 
   , \q r s -> (q :||: r) :.: s :~>  (q :.: s) :||: (r :.: s) 
   ]

ruleCompOverIntersec :: Rule RelAlg
ruleCompOverIntersec = distributionGroup "CompOverIntersec" 
   [ \q r s -> q :.: (r :&&: s) :~> (q :.: r) :&&: (q :.: s)  --alleen toegestaan als q een functie is!
   , \q r s -> (q :&&: r) :.: s :~> (q :.: s) :&&: (r :.: s)  --idem
   ]
ruleAddOverUnion :: Rule RelAlg
ruleAddOverUnion = distributionGroup "AddOverUnion"  
   [ \q r s -> q :+: (r :||: s) :~>  (q :+: r) :||: (q :+: s) --alleen toegestaan als q een functie is!
   , \q r s -> (q :||: r) :+: s :~>  (q :+: s) :||: (r :+: s) --idem
   ]

ruleAddOverIntersec :: Rule RelAlg
ruleAddOverIntersec = distributionGroup "AddOverIntersec"  
   [ \q r s -> q :+: (r :&&: s) :~>  (q :+: r) :&&: (q :+: s)  
   , \q r s -> (q :&&: r) :+: s :~>  (q :+: s) :&&: (r :+: s)  
   ]
-- | 3. Distribute union over intersection
 
ruleUnionOverIntersec :: Rule RelAlg
ruleUnionOverIntersec = distributionGroup "UnionOverIntersec" 
   [ \q r s -> q :||: (r :&&: s) :~> (q :||: r) :&&: (q :||: s) 
   , \q r s -> (q :&&: r) :||: s :~> (q :||: s) :&&: (r :||: s) 
   ]

-- | 4. De Morgan rules

deMorganGroup s = 
   addRuleToGroup (relalg "DeMorgan") . rule s

ruleDeMorganOr :: Rule RelAlg
ruleDeMorganOr = deMorganGroup "DeMorganOr" $
   \r s -> Not (r :||: s) :~> Not r :&&: Not s
   
ruleDeMorganAnd :: Rule RelAlg
ruleDeMorganAnd = deMorganGroup "DeMorganAnd" $
   \r s -> Not (r :&&: s) :~> Not r :||: Not s

-- | 5. Idempotency

idempotencyGroup s = 
   addRuleToGroup (relalg "Idempotency") . rule s

ruleIdempOr :: Rule RelAlg
ruleIdempOr = idempotencyGroup "IdempotencyOr" $
   \r -> r :||: r :~>  r
   
ruleIdempAnd :: Rule RelAlg
ruleIdempAnd = idempotencyGroup "IdempotencyAnd" $
   \r -> r :&&: r :~>  r

-- | 6. Complement

complementGroup s = 
   addRuleToGroup (relalg "Complement") . ruleList s

ruleDoubleNegation :: Rule RelAlg
ruleDoubleNegation = complementGroup "DoubleNegation"
   [ \r -> Not (Not r) :~> r
   ]

ruleRemCompl :: Rule RelAlg
ruleRemCompl = complementGroup "RemCompl" 
   [ \r -> r :||: Not r :~>  V
   , \r -> Not r :||: r :~>  V
   , \r -> r :&&: Not r :~>  empty
   , \r -> Not r :&&: r :~>  empty
   ]   

-- Distribute Not over . and +

ruleNotOverComp :: Rule RelAlg
ruleNotOverComp = complementGroup "NotOverComp"
   [ \r s -> Not (r :.: s) :~> Not r :+: Not s
   ]
   
ruleNotOverAdd :: Rule RelAlg
ruleNotOverAdd = complementGroup "NotOverAdd"
   [ \r s -> Not (r :+: s) :~> Not r :.: Not s
   ]
  
-- | 7. Absorption complement

absorptionGroup s = 
   addRuleToGroup (relalg "Absorption") . ruleList s

ruleAbsorpCompl :: Rule RelAlg
ruleAbsorpCompl = absorptionGroup "AbsorpCompl" 
   [ \r s -> r :&&: (Not r :||: s) :~> r :&&: s
   , \r s -> r :&&: (s :||: Not r) :~> r :&&: s  
   , \r s -> (Not r :||: s) :&&: r :~> r :&&: s
   , \r s -> (s :||: Not r) :&&: r :~> r :&&: s
   , \r s -> r :||: (Not r :&&: s) :~> r :||: s  
   , \r s -> r :||: (s :&&: Not r) :~> r :||: s
   , \r s -> (Not r :&&: s) :||: r :~> r :||: s
   , \r s -> (s :&&: Not r) :||: r :~> r :||: s
   ]
   
ruleAbsorp :: Rule RelAlg
ruleAbsorp = absorptionGroup "Absorp"  
   [ \r s -> r :&&: (r :||: s)  :~> r
   , \r s -> r :&&: (s :||: r)  :~> r
   , \r s -> (r :||: s) :&&: r  :~> r
   , \r s -> (s :||: r) :&&: r  :~> r
   , \r s -> r  :||: (r :&&: s) :~> r
   , \r s -> r  :||: (s :&&: r) :~> r
   , \r s -> (r :&&: s) :||: r  :~> r
   , \r s -> (s :&&: r) :||: r  :~> r
   ]

-- | 8. Remove redundant expressions

simplificationGroup s = 
   addRuleToGroup (relalg "Simplification") . ruleList s

ruleRemRedunExprs :: Rule RelAlg
ruleRemRedunExprs = simplificationGroup "RemRedunExprs"  
   [ \r -> r :||: V :~> V
   , \r -> V :||: r :~> V 
   , \r -> r :&&: V :~> r
   , \r -> V :&&: r :~> r  
--   , (r :.: U)  :~> r
--   , (U :.: r)  :~> r
   , \_ -> V :.: V :~> V
   , \r -> r :+: V :~> V
   , \r -> V :+: r :~> V
--   , (r :+: E)  :~> r
--   , (E :+: r)  :~> r 
   , \_ -> Inv V :~> V
   -- rules involving the empty relation
   , \_ -> Inv empty    :~> empty
   , \r -> r :||: empty :~> r
   , \r -> empty :||: r :~> r 
   , \r -> r :&&: empty :~> empty
   , \r -> empty :&&: r :~> empty
   , \r -> r :.: empty  :~> empty
   , \r -> empty :.: r  :~> empty
   , \_ -> empty :+: empty :~> empty
-- new identity rules: CHECK!
   , \_ -> Inv I :~> I
   , \r -> I :.: r :~> r
   , \r -> r :.: I :~> r
   ]
      
-- Buggy rules:

buggyGroup s = addRuleToGroup (relalg "Buggy") . buggyRule 
             . Rule.ruleList ("relationalgebra.buggy." ++ s)
    
buggyRuleIdemComp :: Rule RelAlg
buggyRuleIdemComp = buggyGroup "IdemComp" 
   [ \q -> q :.: q :~> q 
   ]
    
buggyRuleIdemAdd :: Rule RelAlg
buggyRuleIdemAdd = buggyGroup "IdemAdd"
   [ \q -> q :+: q :~>  q
   ]

buggyRuleDeMorgan :: Rule RelAlg
buggyRuleDeMorgan = buggyGroup "DeMorgan" 
    [ \q r -> Not (q :&&: r) :~> Not q :||: r
    , \q r -> Not (q :&&: r) :~> q :||: Not r
    , \q r -> Not (q :&&: r) :~> Not (Not q :||: Not r)
    , \q r -> Not (q :||: r) :~> Not q :&&: r
    , \q r -> Not (q :||: r) :~> q :&&: Not r
    , \q r -> Not (q :||: r) :~> Not (Not q :&&: Not r) --note the firstNot in both formulas!  
    ]
    
buggyRuleNotOverAdd :: Rule RelAlg
buggyRuleNotOverAdd = buggyGroup "NotOverAdd" 
     [ \q r -> Not (q :+: r) :~> Not q :+: Not r
     , \q r -> Not (q :+: r) :~> Not q :.: r
     , \q r -> Not (q :+: r) :~> Not q :+: r
     , \q r -> Not (q :+: r) :~> Not (Not q :.: Not r) --note the firstNot in both formulas! 
     ]
     
buggyRuleNotOverComp :: Rule RelAlg
buggyRuleNotOverComp = buggyGroup "NotOverComp" 
     [ \q r -> Not (q :.: r) :~> Not q :.: Not r
     , \q r -> Not (q :.: r) :~> Not q :.: r
     , \q r -> Not (q :.: r) :~> Not q :+: r
     , \q r -> Not (q :.: r) :~> Not (Not q :.: Not r) --note the firstNot in both formulas! 
     ]
     
buggyRuleParenth :: Rule RelAlg
buggyRuleParenth = buggyGroup "Parenth" 
    [ \q r -> Not (q :&&: r)     :~> Not q :&&: r
    , \q r -> Not (q :||: r)     :~> Not q :||: r
    , \q r -> Not (Not q :&&: r) :~> q :&&: r 
    , \q r -> Not (Not q :||: r) :~> q :||: r
    , \q r -> Not (Not q :.: r)  :~> q :.: r
    , \q r -> Not (Not q :+: r)  :~> q :+: r
    , \q r -> Inv (q :&&: r)     :~> Inv q :&&: r
    , \q r -> Inv (q :||: r)     :~> Inv q :||: r
    , \q r -> Inv (Inv q :&&: r) :~> q :&&: r 
    , \q r -> Inv (Inv q :||: r) :~> q :||: r
    , \q r -> Inv (Inv q :.: r)  :~> q :.: r
    , \q r -> Inv (Inv q :+: r)  :~> q :+: r
    ]
    
buggyRuleAssoc :: Rule RelAlg
buggyRuleAssoc = buggyGroup "Assoc"  
    [ \q r s -> q :||: (r :&&: s) :~> (q :||: r) :&&: s
    , \q r s -> (q :||: r) :&&: s :~> q :||: (r :&&: s)
    , \q r s -> (q :&&: r) :||: s :~> q :&&: (r :||: s)
    , \q r s -> q :&&: (r :||: s) :~> (q :&&: r) :||: s
    , \q r s -> q :.: (r :||: s)  :~> (q :.: r) :||: s 
    , \q r s -> (q :||: r) :.: s  :~> q :||: (r :.: s) 
    , \q r s -> q :.: (r :&&: s)  :~> (q :.: r) :&&: s 
    , \q r s -> (q :&&: r) :.: s  :~> q :&&: (r :.: s) 
    , \q r s -> q :+: (r :||: s)  :~> (q :+: r) :||: s 
    , \q r s -> (q :||: r) :+: s  :~> q :||: (r :+: s) 
    , \q r s -> q :+: (r :&&: s)  :~> (q :+: r) :&&: s  
    , \q r s -> (q :&&: r) :+: s  :~> q :&&: (r :+: s)
    ]

buggyRuleInvOverComp :: Rule RelAlg
buggyRuleInvOverComp = buggyGroup "InvOverComp"
   [ \r s -> Inv (r :.: s) :~> Inv r :.: Inv s
   ]

buggyRuleInvOverAdd :: Rule RelAlg
buggyRuleInvOverAdd = buggyGroup "InvOverAdd"
   [ \r s -> Inv (r :+: s) :~> Inv r :+: Inv s
   ]
   
buggyRuleCompOverIntersec :: Rule RelAlg
buggyRuleCompOverIntersec = buggyGroup "CompOverIntersec" 
   [ \q r s -> q :.: (r :&&: s) :~> (q :.: r) :&&: (q :.: s)  --alleen toegestaan als q een functie is!
   , \q r s -> (q :&&: r) :.: s :~> (q :.: s) :&&: (r :.: s)  --idem
   ]
buggyRuleAddOverUnion :: Rule RelAlg
buggyRuleAddOverUnion = buggyGroup "AddOverUnion" 
   [ \q r s -> q :+: (r :||: s) :~> (q :+: r) :||: (q :+: s) --alleen toegestaan als q een functie is!
   , \q r s -> (q :||: r) :+: s :~> (q :+: s) :||: (r :+: s) --idem
   ]
   
buggyRuleRemCompl :: Rule RelAlg
buggyRuleRemCompl = buggyGroup "RemCompl" 
   [ \r -> r :&&: Not r :~> V
   , \r -> Not r :&&: r :~> V
   , \r -> r :||: Not r :~> empty
   , \r -> Not r :||: r :~> empty
   ]

-- Older rules involving the empty relation
{-
  -- RemRedunExprs
   \_ -> (Not V)    :~> E
   \_ -> (Not E)    :~> V
-}