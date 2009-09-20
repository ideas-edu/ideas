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
-- Rewrite rules in the logic domain (including all the rules from the DWA course)
--
-----------------------------------------------------------------------------
module Domain.Logic.Rules where

import Domain.Logic.Formula
import Common.Transformation
import Common.Rewriting
import Domain.Logic.Generator()
 
logicRules :: [Rule SLogic]
logicRules = concat 
   [ groupCommutativity, groupAssociativity, groupDistributivity, groupIdempotency
   , groupAbsorption, groupTrueProperties, groupFalseProperties, groupDoubleNegation
   , groupDeMorgan, groupImplicationEliminatinon, groupEquivalenceElimination, groupAdditional
   ]

buggyRules :: [Rule SLogic]
buggyRules = makeGroup "Common misconceptions"
   [ buggyRuleCommImp, buggyRuleAssImp, buggyRuleIdemImp, buggyRuleIdemEqui
   , buggyRuleEquivElim1, buggyRuleEquivElim2
   , buggyRuleImplElim, buggyRuleImplElim1, buggyRuleDeMorgan1, buggyRuleDeMorgan2, buggyRuleDeMorgan3
   , buggyRuleDeMorgan4, buggyRuleNotOverImpl, buggyRuleParenth1, buggyRuleParenth2
   , buggyRuleParenth3, buggyRuleAssoc
   , buggyRuleAndSame, buggyRuleAndCompl, buggyRuleOrSame, buggyRuleOrCompl
   , buggyRuleTrueProp, buggyRuleFalseProp, buggyRuleDistr
   ]

-----------------------------------------------------------------------------
-- Grouping DWA rules

makeGroup :: String -> [Rule SLogic] -> [Rule SLogic]
makeGroup = map . addRuleToGroup

groupCommutativity, groupAssociativity, groupDistributivity, groupIdempotency, 
   groupAbsorption, groupTrueProperties, groupFalseProperties, groupDoubleNegation,
   groupDeMorgan, groupImplicationEliminatinon, groupEquivalenceElimination :: [Rule SLogic]

groupCommutativity = makeGroup "Commutativity" 
   [ruleCommOr, ruleCommAnd]
groupAssociativity = makeGroup "Associativity"
   [ruleAssocOr, ruleAssocAnd]
groupDistributivity = makeGroup "Distributivity"
   [ruleAndOverOr, ruleOrOverAnd]
groupIdempotency = makeGroup "Idempotency"
   [ruleIdempOr, ruleIdempAnd]
groupAbsorption = makeGroup "Absorption"
   [ruleAbsorpOr, ruleAbsorpAnd]
groupTrueProperties = makeGroup "True Properties"
   [ruleTrueZeroOr, ruleTrueZeroAnd, ruleComplOr, ruleNotTrue]
groupFalseProperties = makeGroup "False Properties"
   [ruleFalseZeroOr, ruleFalseZeroAnd, ruleComplAnd, ruleNotFalse]
groupDoubleNegation = makeGroup "Double Negation"
   [ruleNotNot]
groupDeMorgan = makeGroup "De Morgan" 
   [ruleDeMorganOr, ruleDeMorganAnd]
groupImplicationEliminatinon = makeGroup "Implication Elimination"
   [ruleDefImpl]
groupEquivalenceElimination = makeGroup "Equivalence Elimination"
   [ruleDefEquiv]
   
-----------------------------------------------------------------------------
-- Commutativity

ruleCommOr, ruleCommAnd :: Rule SLogic 

ruleCommOr = rule "CommOr" $
   \x y -> x :||: y  :~>  y :||: x

ruleCommAnd = rule "CommAnd" $
   \x y -> x :&&: y  :~>  y :&&: x
   
-----------------------------------------------------------------------------
-- Associativity (implicit)

ruleAssocOr, ruleAssocAnd :: Rule SLogic

ruleAssocOr = minorRule $ rule "AssocOr" $
   \x y z -> (x :||: y) :||: z  :~>  x :||: (y :||: z)
     
ruleAssocAnd = minorRule $ rule "AssocAnd" $
   \x y z -> (x :&&: y) :&&: z  :~>  x :&&: (y :&&: z)
   
-----------------------------------------------------------------------------
-- Distributivity

ruleAndOverOr, ruleOrOverAnd :: Rule SLogic 

ruleAndOverOr = ruleList "AndOverOr"
   [ \x y z -> x :&&: (y :||: z)  :~>  (x :&&: y) :||: (x :&&: z)
   , \x y z -> (x :||: y) :&&: z  :~>  (x :&&: z) :||: (y :&&: z)
   ]

ruleOrOverAnd = ruleList "OrOverAnd"
   [ \x y z -> x :||: (y :&&: z)  :~>  (x :||: y) :&&: (x :||: z)
   , \x y z -> (x :&&: y) :||: z  :~>  (x :||: z) :&&: (y :||: z)
   ]
   
-----------------------------------------------------------------------------
-- Idempotency

ruleIdempOr, ruleIdempAnd :: Rule SLogic 

ruleIdempOr = rule "IdempOr" $
   \x -> x :||: x  :~>  x

ruleIdempAnd = rule "IdempAnd" $
   \x -> x :&&: x  :~>  x

-----------------------------------------------------------------------------
-- Absorption

ruleAbsorpOr, ruleAbsorpAnd :: Rule SLogic 

ruleAbsorpOr = ruleList "AbsorpOr" 
   [ \x y -> x :||: (x :&&: y)  :~>  x
   , \x y -> x :||: (y :&&: x)  :~>  x
   , \x y -> (x :&&: y) :||: x  :~>  x
   , \x y -> (y :&&: x) :||: x  :~>  x
   ]
    
ruleAbsorpAnd = ruleList "AbsorpAnd"
   [ \x y -> x :&&: (x :||: y)  :~>  x 
   , \x y -> x :&&: (y :||: x)  :~>  x 
   , \x y -> (x :||: y) :&&: x  :~>  x 
   , \x y -> (y :||: x) :&&: x  :~>  x 
   ]

-----------------------------------------------------------------------------
-- True-properties

ruleTrueZeroOr, ruleTrueZeroAnd, ruleComplOr, ruleNotTrue :: Rule SLogic 

ruleTrueZeroOr = ruleList "TrueZeroOr"
   [ \x -> T :||: x  :~>  T
   , \x -> x :||: T  :~>  T
   ]

ruleTrueZeroAnd = ruleList "TrueZeroAnd"
   [ \x -> T :&&: x  :~>  x
   , \x -> x :&&: T  :~>  x
   ] 
 
ruleComplOr = ruleList "ComplOr"
   [ \x -> x :||: Not x  :~>  T
   , \x -> Not x :||: x  :~>  T
   ]

ruleNotTrue = rule "NotTrue" $
   Not T  :~>  F
   
-----------------------------------------------------------------------------
-- False-properties

ruleFalseZeroOr, ruleFalseZeroAnd, ruleComplAnd, ruleNotFalse :: Rule SLogic 

ruleFalseZeroOr = ruleList "FalseZeroOr"
   [ \x -> F :||: x  :~>  x
   , \x -> x :||: F  :~>  x
   ]
  
ruleFalseZeroAnd = ruleList "FalseZeroAnd"
   [ \x -> F :&&: x  :~>  F
   , \x -> x :&&: F  :~>  F
   ]
 
ruleComplAnd = ruleList "ComplAnd"
   [ \x -> x :&&: Not x  :~>  F
   , \x -> Not x :&&: x  :~>  F
   ]

ruleNotFalse = rule "NotFalse" $
   Not F  :~>  T

-----------------------------------------------------------------------------
-- Double negation

ruleNotNot :: Rule SLogic 
ruleNotNot = rule "NotNot" $
   \x -> Not (Not x)  :~>  x
   
-----------------------------------------------------------------------------
-- De Morgan

ruleDeMorganOr, ruleDeMorganAnd :: Rule SLogic 

ruleDeMorganOr = rule "DeMorganOr" $
   \x y -> Not (x :||: y)  :~>  Not x :&&: Not y

ruleDeMorganAnd = rule "DeMorganAnd" $
   \x y -> Not (x :&&: y)  :~>  Not x :||: Not y
   
-----------------------------------------------------------------------------
-- Implication elimination

ruleDefImpl :: Rule SLogic 
ruleDefImpl = rule "DefImpl" $
   \x y -> x :->: y  :~>  Not x :||: y
   
-----------------------------------------------------------------------------
-- Equivalence elimination

ruleDefEquiv :: Rule SLogic 
ruleDefEquiv = rule "DefEquiv" $
   \x y -> x :<->: y  :~>  (x :&&: y) :||: (Not x :&&: Not y)

-----------------------------------------------------------------------------
-- Additional rules, not in the DWA course

groupAdditional :: [Rule SLogic]
groupAdditional = makeGroup "Additional rules"
   [ ruleFalseInEquiv, ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
   , ruleCommEquiv, ruleDefEquivImpls, ruleEquivSame, ruleImplSame
   ]

ruleFalseInEquiv :: Rule SLogic 
ruleFalseInEquiv = ruleList "FalseInEquiv"
   [ \x -> F :<->: x  :~>  Not x
   , \x -> x :<->: F  :~>  Not x
   ]
   
ruleTrueInEquiv :: Rule SLogic 
ruleTrueInEquiv = ruleList "TrueInEquiv"
   [ \x -> T :<->: x  :~>  x
   , \x -> x :<->: T  :~>  x
   ]

ruleFalseInImpl :: Rule SLogic 
ruleFalseInImpl = ruleList "FalseInImpl"
   [ \x -> F :->: x  :~>  T
   , \x -> x :->: F  :~> Not x
   ]
   
ruleTrueInImpl :: Rule SLogic 
ruleTrueInImpl = ruleList "TrueInImpl"
   [ \x -> T :->: x  :~>  x
   , \x -> x :->: T  :~>  T
   ]
        
ruleCommEquiv :: Rule SLogic 
ruleCommEquiv = rule "CommEquiv" $
   \x y -> x :<->: y  :~>  y :<->: x

ruleDefEquivImpls :: Rule SLogic 
ruleDefEquivImpls = rule "DefEquivImpls" $
   \x y -> x :<->: y  :~>  (x :->: y) :&&: (y :->: x)

ruleEquivSame :: Rule SLogic 
ruleEquivSame = rule "EquivSame" $
   \x -> x :<->: x  :~>  T

ruleImplSame :: Rule SLogic 
ruleImplSame = rule "ImplSame" $
   \x -> x :->: x  :~>  T
   
-----------------------------------------------------------------------------
-- Buggy rules

buggyRuleAndSame :: Rule SLogic
buggyRuleAndSame = buggyRule $ rule "AndSame" $
   \x -> x :&&: x  :~>  T

buggyRuleAndCompl :: Rule SLogic
buggyRuleAndCompl = buggyRule $ ruleList "AndComplBuggy"
   [ \x -> x :&&: Not x  :~>  T
   , \x -> Not x :&&: x  :~>  T
   , \x -> x :&&: Not x  :~>  x
   , \x -> Not x :&&: x  :~>  x
   ]
   
buggyRuleOrSame :: Rule SLogic
buggyRuleOrSame = buggyRule $ rule "OrSame" $
   \x -> x :||: x  :~>  T

buggyRuleOrCompl :: Rule SLogic
buggyRuleOrCompl = buggyRule $ ruleList "OrComplBuggy"
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
buggyRuleEquivElim1 = buggyRule $ ruleList "BuggyEquivElim1"
    [ \x y -> x :<->: y :~> (x :&&: y) :||: Not (x :&&: y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: (Not x :&&:  y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: ( x :&&: Not y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: (x :&&: y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: Not (x :||: Not y)
    ]
    
buggyRuleEquivElim2 :: Rule SLogic
buggyRuleEquivElim2 = buggyRule $ ruleList "BuggyEquivElim2"
    [ \x y -> x :<->: y :~> (x :||: y) :&&: (Not x :||: Not y)
    , \x y -> x :<->: y :~> (x :&&: y) :&&: (Not x :&&: Not y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: (Not x :||: Not y)
    ]
    
buggyRuleImplElim :: Rule SLogic
buggyRuleImplElim = buggyRule $ ruleList "BuggyImplElim" 
   [\x y -> x :->: y :~> Not (x :||: y)
   ,\x y -> x :->: y :~> (x :||: y)
   ,\x y -> x :->: y :~> Not (x :&&: y)
   ]
  
buggyRuleImplElim1 :: Rule SLogic
buggyRuleImplElim1 = buggyRule $ rule "BuggyImplElim1"  $  
     \x y -> x :->: y :~> Not x :&&: y
     
buggyRuleDeMorgan1 :: Rule SLogic
buggyRuleDeMorgan1 = buggyRule $ ruleList "BuggyDeMorgan1"
    [ \x y -> Not (x :&&: y) :~>  Not x :||: y
    , \x y -> Not (x :&&: y) :~>  x :||: Not y
    , \x y -> Not (x :&&: y) :~>  x :||: y
    , \x y -> Not (x :||: y) :~>  Not x :&&: y
    , \x y -> Not (x :||: y) :~>  x :&&: Not y 
    , \x y -> Not (x :||: y) :~>  x :&&: y
    ]
    
buggyRuleDeMorgan2 :: Rule SLogic
buggyRuleDeMorgan2 = buggyRule $ ruleList "BuggyDeMorgan2"
    [ \x y -> Not (x :&&: y) :~>  Not (Not x :||: Not y)
    , \x y -> Not (x :||: y) :~>  Not (Not x :&&: Not y) --note the firstNot in both formulas!  
    ]
buggyRuleDeMorgan3 :: Rule SLogic    
buggyRuleDeMorgan3 = buggyRule $  rule "BuggyDeMorgan3" $
    \x y -> Not (x :&&: y) :~>  Not x :&&: Not y

buggyRuleDeMorgan4 :: Rule SLogic    
buggyRuleDeMorgan4 = buggyRule $  rule "BuggyDeMorgan4" $   
     \x y -> Not (x :||: y) :~>  Not x :||: Not y
 
buggyRuleNotOverImpl :: Rule SLogic
buggyRuleNotOverImpl = buggyRule $ rule "BuggyNotOverImpl" $
    \x y -> Not (x :->: y) :~> Not x :->: Not y
    
buggyRuleParenth1 :: Rule SLogic
buggyRuleParenth1 = buggyRule $ ruleList "BuggyParenth1"
    [ \x y -> Not (x :&&: y)     :~> Not x :&&: y
    , \x y -> Not (x :||: y)     :~> Not x :||: y
    ]

buggyRuleParenth2 :: Rule SLogic
buggyRuleParenth2 = buggyRule $ rule "BuggyParenth2" $
    \x y -> Not (x :<->: y) :~> Not(x :&&: y) :||: (Not x :&&: Not y)
    
buggyRuleParenth3 :: Rule SLogic
buggyRuleParenth3 = buggyRule $ ruleList "BuggyParenth3"    
    [ \x y -> Not (Not x :&&: y)  :~> x :&&: y 
    , \x y -> Not (Not x :||: y)  :~> x :||: y
    , \x y -> Not (Not x :->: y)  :~> x :->: y
    , \x y -> Not (Not x :<->: y) :~> x :<->: y
    ]
   
        
buggyRuleAssoc :: Rule SLogic
buggyRuleAssoc = buggyRule $ ruleList "BuggyAssoc"
    [ \x y z -> x :||: (y :&&: z) :~> (x :||: y) :&&: z
    , \x y z -> (x :||: y) :&&: z :~> x :||: (y :&&: z)
    , \x y z -> (x :&&: y) :||: z :~> x :&&: (y :||: z)
    , \x y z -> x :&&: (y :||: z) :~> (x :&&: y) :||: z
    ]
    
buggyRuleDistr :: Rule SLogic
buggyRuleDistr = buggyRule $ ruleList "BuggyDistr"
   [ \x y z -> x :&&: (y :||: z)  :~>  (x :&&: y) :&&: (x :&&: z)
   , \x y z -> (x :||: y) :&&: z  :~>  (x :&&: z) :&&: (y :&&: z)
   , \x y z -> x :&&: (y :||: z)  :~>  (x :||: y) :&&: (x :||: z)
   , \x y z -> (x :||: y) :&&: z  :~>  (x :||: z) :&&: (y :||: z)
   , \x y z -> x :||: (y :&&: z)  :~>  (x :||: y) :||: (x :||: z)
   , \x y z -> (x :&&: y) :||: z  :~>  (x :||: z) :||: (y :||: z)
   , \x y z -> x :||: (y :&&: z)  :~>  (x :&&: y) :||: (x :&&: z)
   , \x y z -> (x :&&: y) :||: z  :~>  (x :&&: z) :||: (y :&&: z)
   ] 