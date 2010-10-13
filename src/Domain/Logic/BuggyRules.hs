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
-- Buggy rules in the logic domain, expressing common misconceptions
--
-----------------------------------------------------------------------------
module Domain.Logic.BuggyRules where

import Domain.Logic.Formula
import Domain.Logic.Generator()
import Domain.Logic.Rules (makeGroup, logic)
import Common.Id
import Common.Rewriting
import Common.Transformation (Rule, buggyRule)
import qualified Common.Transformation as Rule

-- Collection of all known buggy rules
buggyRules :: [Rule SLogic]
buggyRules = snd $ makeGroup "Common misconceptions"
   [ buggyRuleCommImp, buggyRuleAssImp, buggyRuleIdemImp, buggyRuleIdemEqui
   , buggyRuleEquivElim1, buggyRuleImplElim2, buggyRuleEquivElim2, buggyRuleEquivElim3
   , buggyRuleImplElim, buggyRuleImplElim1, buggyRuleDeMorgan1, buggyRuleDeMorgan2, buggyRuleDeMorgan3
   , buggyRuleDeMorgan4, buggyRuleDeMorgan5, buggyRuleNotOverImpl, buggyRuleParenth1, buggyRuleParenth2
   , buggyRuleParenth3, buggyRuleAssoc, buggyRuleAbsor
   , buggyRuleAndSame, buggyRuleAndCompl, buggyRuleOrSame, buggyRuleOrCompl
   , buggyRuleTrueProp, buggyRuleFalseProp, buggyRuleDistr, buggyRuleDistrNot
   ]

rule :: (Builder f a, Rewrite a) => String -> f -> Rule a
rule = Rule.rule . logic . ("buggy" #)

ruleList :: (Builder f a, Rewrite a) => String -> [f] -> Rule a
ruleList = Rule.ruleList . logic . ("buggy" #)

-----------------------------------------------------------------------------
-- Buggy rules

buggyRuleAndSame :: Rule SLogic
buggyRuleAndSame = buggyRule $ rule "AndSame" $
   \x -> x :&&: x  :~>  T

buggyRuleAndCompl :: Rule SLogic
buggyRuleAndCompl = buggyRule $ ruleList "AndCompl"
   [ \x -> x :&&: Not x  :~>  T
   , \x -> Not x :&&: x  :~>  T
   , \x -> x :&&: Not x  :~>  x
   , \x -> Not x :&&: x  :~>  x
   ]
   
buggyRuleOrSame :: Rule SLogic
buggyRuleOrSame = buggyRule $ rule "OrSame" $
   \x -> x :||: x  :~>  T

buggyRuleOrCompl :: Rule SLogic
buggyRuleOrCompl = buggyRule $ ruleList "OrCompl"
   [ \x -> x :||: Not x  :~>  F
   , \x -> Not x :||:  x :~>  F
   , \x -> x :||: Not x  :~>  x
   , \x -> Not x :||:  x :~>  x
   ]
    
buggyRuleTrueProp :: Rule SLogic
buggyRuleTrueProp = buggyRule $ ruleList "TrueProp" 
   [ \x -> x :||: T  :~>  x
   , \x -> T :||: x  :~>  x
   , \x -> x :&&: T  :~>  T
   , \x -> T :&&: x  :~>  T
   ]

buggyRuleFalseProp :: Rule SLogic
buggyRuleFalseProp = buggyRule $ ruleList "FalseProp" 
   [ \x -> x :||: F  :~>  F
   , \x -> F :||: x  :~>  F
   , \x -> x :&&: F  :~>  x
   , \x -> F :&&: x  :~>  x
   ]

buggyRuleCommImp :: Rule SLogic 
buggyRuleCommImp = buggyRule $ rule "CommImp" $
   \x y -> x :->: y  :~>  y :->: x --this does not hold: T->T => T->x

buggyRuleAssImp :: Rule SLogic
buggyRuleAssImp = buggyRule $ ruleList "AssImp"
   [ \x y z -> x :->: (y :->: z)  :~>  (x :->: y) :->: z
   , \x y z -> (x :->: y) :->: z  :~>  x :->: (y :->: z)
   ]
    
buggyRuleIdemImp :: Rule SLogic
buggyRuleIdemImp = buggyRule $ rule "IdemImp" $
   \x -> x :->: x  :~>  x 
    
buggyRuleIdemEqui :: Rule SLogic
buggyRuleIdemEqui = buggyRule $ rule "IdemEqui" $
   \x -> x :<->: x  :~>  x 

buggyRuleEquivElim1 :: Rule SLogic
buggyRuleEquivElim1 = buggyRule $ ruleList "EquivElim1"
    [ \x y -> x :<->: y :~> (x :&&: y) :||: Not (x :&&: y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: (Not x :&&:  y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: ( x :&&: Not y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: (x :&&: y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: Not (x :||: Not y)
    ]
    
buggyRuleEquivElim2 :: Rule SLogic
buggyRuleEquivElim2 = buggyRule $ ruleList "EquivElim2"
    [ \x y -> x :<->: y :~> (x :||: y) :&&: (Not x :||: Not y)
    , \x y -> x :<->: y :~> (x :&&: y) :&&: (Not x :&&: Not y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: (Not x :||: Not y)
    ]
    
buggyRuleEquivElim3 :: Rule SLogic
buggyRuleEquivElim3 = buggyRule $ rule "EquivElim3"  $  
     \x y -> x :<->: y :~> Not x :||: y
    
buggyRuleImplElim :: Rule SLogic
buggyRuleImplElim = buggyRule $ ruleList "ImplElim" 
   [\x y -> x :->: y :~> Not (x :||: y)
   ,\x y -> x :->: y :~> (x :||: y)
   ,\x y -> x :->: y :~> Not (x :&&: y)
   ]
  
buggyRuleImplElim1 :: Rule SLogic
buggyRuleImplElim1 = buggyRule $ rule "ImplElim1"  $  
     \x y -> x :->: y :~> Not x :&&: y

buggyRuleImplElim2 :: Rule SLogic
buggyRuleImplElim2 = buggyRule $ rule "ImplElim2" $ 
     \x y -> x :->: y :~>  (x :&&: y) :||: (Not x :&&: Not y) 
     
buggyRuleDeMorgan1 :: Rule SLogic
buggyRuleDeMorgan1 = buggyRule $ ruleList "DeMorgan1"
    [ \x y -> Not (x :&&: y) :~>  Not x :||: y
    , \x y -> Not (x :&&: y) :~>  x :||: Not y
    , \x y -> Not (x :&&: y) :~>  x :||: y
    , \x y -> Not (x :||: y) :~>  Not x :&&: y
    , \x y -> Not (x :||: y) :~>  x :&&: Not y 
    , \x y -> Not (x :||: y) :~>  x :&&: y
    ]
    
buggyRuleDeMorgan2 :: Rule SLogic
buggyRuleDeMorgan2 = buggyRule $ ruleList "DeMorgan2"
    [ \x y -> Not (x :&&: y) :~>  Not (Not x :||: Not y)
    , \x y -> Not (x :||: y) :~>  Not (Not x :&&: Not y) --note the firstNot in both formulas!  
    ]
buggyRuleDeMorgan3 :: Rule SLogic    
buggyRuleDeMorgan3 = buggyRule $  rule "DeMorgan3" $
    \x y -> Not (x :&&: y) :~>  Not x :&&: Not y

buggyRuleDeMorgan4 :: Rule SLogic    
buggyRuleDeMorgan4 = buggyRule $  rule "DeMorgan4" $   
     \x y -> Not (x :||: y) :~>  Not x :||: Not y
     
buggyRuleDeMorgan5 :: Rule SLogic
buggyRuleDeMorgan5 = buggyRule $ ruleList "DeMorgan5"
    [ \x y z -> Not (Not (x :&&: y) :||: z) :~>  Not (Not x :||: Not y):||: z
    , \x y z -> Not (Not (x :&&: y) :&&: z) :~>  Not (Not x :||: Not y):&&: z
    , \x y z -> Not (Not (x :||: y) :||: z) :~>  Not (Not x :&&: Not y):||: z
    , \x y z -> Not (Not (x :||: y) :&&: z) :~>  Not (Not x :&&: Not y):&&: z 
    ] 
    
buggyRuleNotOverImpl :: Rule SLogic
buggyRuleNotOverImpl = buggyRule $ rule "NotOverImpl" $
    \x y -> Not (x :->: y) :~> Not x :->: Not y
    
buggyRuleParenth1 :: Rule SLogic
buggyRuleParenth1 = buggyRule $ ruleList "Parenth1"
    [ \x y -> Not (x :&&: y)     :~> Not x :&&: y
    , \x y -> Not (x :||: y)     :~> Not x :||: y
    ]

buggyRuleParenth2 :: Rule SLogic
buggyRuleParenth2 = buggyRule $ rule "Parenth2" $
    \x y -> Not (x :<->: y) :~> Not(x :&&: y) :||: (Not x :&&: Not y)
    
buggyRuleParenth3 :: Rule SLogic
buggyRuleParenth3 = buggyRule $ ruleList "Parenth3"    
    [ \x y -> Not (Not x :&&: y)  :~> x :&&: y 
    , \x y -> Not (Not x :||: y)  :~> x :||: y
    , \x y -> Not (Not x :->: y)  :~> x :->: y
    , \x y -> Not (Not x :<->: y) :~> x :<->: y
    ]
   
        
buggyRuleAssoc :: Rule SLogic
buggyRuleAssoc = buggyRule $ ruleList "Assoc"
    [ \x y z -> x :||: (y :&&: z) :~> (x :||: y) :&&: z
    , \x y z -> (x :||: y) :&&: z :~> x :||: (y :&&: z)
    , \x y z -> (x :&&: y) :||: z :~> x :&&: (y :||: z)
    , \x y z -> x :&&: (y :||: z) :~> (x :&&: y) :||: z
    ]
 
buggyRuleAbsor :: Rule SLogic
buggyRuleAbsor = buggyRule $ ruleList "Absor"
    [ \x y z -> (x :||: y) :||: ((x :&&: y) :&&: z) :~> (x :||: y) 
    , \x y z -> (x :&&: y) :||: ((x :||: y) :&&: z) :~> (x :&&: y) 
    , \x y z -> (x :||: y) :&&: ((x :&&: y) :||: z) :~> (x :||: y) 
    , \x y z -> (x :&&: y) :&&: ((x :||: y) :||: z) :~> (x :&&: y) 
    ]
    
buggyRuleDistr :: Rule SLogic
buggyRuleDistr = buggyRule $ ruleList "Distr"
   [ \x y z -> x :&&: (y :||: z)  :~>  (x :&&: y) :&&: (x :&&: z)
   , \x y z -> (x :||: y) :&&: z  :~>  (x :&&: z) :&&: (y :&&: z)
   , \x y z -> x :&&: (y :||: z)  :~>  (x :||: y) :&&: (x :||: z)
   , \x y z -> (x :||: y) :&&: z  :~>  (x :||: z) :&&: (y :||: z)
   , \x y z -> x :||: (y :&&: z)  :~>  (x :||: y) :||: (x :||: z)
   , \x y z -> (x :&&: y) :||: z  :~>  (x :||: z) :||: (y :||: z)
   , \x y z -> x :||: (y :&&: z)  :~>  (x :&&: y) :||: (x :&&: z)
   , \x y z -> (x :&&: y) :||: z  :~>  (x :&&: z) :||: (y :&&: z)
   ] 
   
buggyRuleDistrNot :: Rule SLogic
buggyRuleDistrNot = buggyRule $ ruleList "DistrNot"
   [ \x y z -> Not x :&&: (y :||: z)  :~>  (Not x :&&: y) :||: (x :&&: z)
   , \x y z -> Not x :&&: (y :||: z)  :~>  (x :&&: y) :||: (Not x :&&: z)
   , \x y z -> (x :||: y) :&&: Not z  :~>  (x :&&: Not z) :||: (y :&&: z)
   , \x y z -> (x :||: y) :&&: Not z  :~>  (x :&&: z) :||: (y :&&: Not z)
   , \x y z -> Not x :||: (y :&&: z)  :~>  (Not x :||: y) :&&: (x :||: z)
   , \x y z -> Not x :||: (y :&&: z)  :~>  (x :||: y) :&&: (Not x :||: z)
   , \x y z -> (x :&&: y) :||: Not z  :~>  (x :||: Not z) :&&: (y :||: z)
   , \x y z -> (x :&&: y) :||: Not z  :~>  (x :||: z) :&&: (y :||: Not z)
   ]  