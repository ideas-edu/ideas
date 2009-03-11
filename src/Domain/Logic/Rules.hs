{-# OPTIONS -fglasgow-exts #-}
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
 
logicRules :: [Rule Logic]
logicRules = concat 
   [ groupCommutativity, groupAssociativity, groupDistributivity, groupIdempotency
   , groupAbsorption, groupTrueProperties, groupFalseProperties, groupDoubleNegation
   , groupDeMorgan, groupImplicationEliminatinon, groupEquivalenceElimination, groupAdditional
   ]

buggyRules :: [Rule Logic]
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

makeGroup :: String -> [Rule Logic] -> [Rule Logic]
makeGroup = map . addRuleToGroup

groupCommutativity, groupAssociativity, groupDistributivity, groupIdempotency, 
   groupAbsorption, groupTrueProperties, groupFalseProperties, groupDoubleNegation,
   groupDeMorgan, groupImplicationEliminatinon, groupEquivalenceElimination :: [Rule Logic]

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

ruleCommOr, ruleCommAnd :: Rule Logic 

ruleCommOr = rule "CommOr" $
   \x y -> (x :||: y)  :~>  (y :||: x) 
     
ruleCommAnd = rule "CommAnd" $
   \x y -> (x :&&: y)  :~>  (y :&&: x)
   
-----------------------------------------------------------------------------
-- Associativity (implicit)

ruleAssocOr, ruleAssocAnd :: Rule Logic

ruleAssocOr = minorRule $ rule "AssocOr" $
   \x y z -> ((x :||: y) :||: z)  :~>  (x :||: (y :||: z)) 
     
ruleAssocAnd = minorRule $ rule "AssocAnd" $
   \x y z -> ((x :&&: y) :&&: z)  :~>  (x :&&: (y :&&: z))
   
-----------------------------------------------------------------------------
-- Distributivity

ruleAndOverOr, ruleOrOverAnd :: Rule Logic 

ruleAndOverOr = ruleList "AndOverOr"
   [ \x y z -> (x :&&: (y :||: z))  :~>  ((x :&&: y) :||: (x :&&: z))
   , \x y z -> ((x :||: y) :&&: z)  :~>  ((x :&&: z) :||: (y :&&: z))
   ]

ruleOrOverAnd = ruleList "OrOverAnd"
   [ \x y z -> (x :||: (y :&&: z))  :~>  ((x :||: y) :&&: (x :||: z))
   , \x y z -> ((x :&&: y) :||: z)  :~>  ((x :||: z) :&&: (y :||: z))
   ]
   
-----------------------------------------------------------------------------
-- Idempotency

ruleIdempOr, ruleIdempAnd :: Rule Logic 

ruleIdempOr = rule "IdempOr" $
   \x -> (x :||: x)  :~>  x

ruleIdempAnd = rule "IdempAnd" $
   \x -> (x :&&: x)  :~>  x

-----------------------------------------------------------------------------
-- Absorption

ruleAbsorpOr, ruleAbsorpAnd :: Rule Logic 

ruleAbsorpOr = ruleList "AbsorpOr" 
   [ \x y -> (x :||: (x :&&: y))  :~>  x
   , \x y -> (x :||: (y :&&: x))  :~>  x
   , \x y -> ((x :&&: y) :||: x)  :~>  x
   , \x y -> ((y :&&: x) :||: x)  :~>  x
   ]
    
ruleAbsorpAnd = ruleList "AbsorpAnd"
   [ \x y -> (x :&&: (x :||: y))  :~>  x 
   , \x y -> (x :&&: (y :||: x))  :~>  x 
   , \x y -> ((x :||: y) :&&: x)  :~>  x 
   , \x y -> ((y :||: x) :&&: x)  :~>  x 
   ]

-----------------------------------------------------------------------------
-- True-properties

ruleTrueZeroOr, ruleTrueZeroAnd, ruleComplOr, ruleNotTrue :: Rule Logic 

ruleTrueZeroOr = ruleList "TrueZeroOr"
   [ \x -> (T :||: x)  :~>  T
   , \x -> (x :||: T)  :~>  T
   ]

ruleTrueZeroAnd = ruleList "TrueZeroAnd"
   [ \x -> (T :&&: x)  :~>  x
   , \x -> (x :&&: T)  :~>  x
   ] 
 
ruleComplOr = ruleList "ComplOr" $
   [ \x -> (x :||: Not x)  :~>  T
   , \x -> (Not x :||: x)  :~>  T
   ]

ruleNotTrue = rule "NotTrue" $
   (Not T)  :~>  F
   
-----------------------------------------------------------------------------
-- False-properties

ruleFalseZeroOr, ruleFalseZeroAnd, ruleComplAnd, ruleNotFalse :: Rule Logic 

ruleFalseZeroOr = ruleList "FalseZeroOr"
   [ \x -> (F :||: x)  :~>  x
   , \x -> (x :||: F)  :~>  x
   ]
  
ruleFalseZeroAnd = ruleList "FalseZeroAnd"
   [ \x -> (F :&&: x)  :~>  F
   , \x -> (x :&&: F)  :~>  F
   ]
 
ruleComplAnd = ruleList "ComplAnd"
   [ \x -> (x :&&: Not x)  :~>  F
   , \x -> (Not x :&&: x)  :~>  F
   ]

ruleNotFalse = rule "NotFalse" $
   (Not F)  :~>  T

-----------------------------------------------------------------------------
-- Double negation

ruleNotNot :: Rule Logic 
ruleNotNot = rule "NotNot" $
   \x -> (Not (Not x))  :~>  x
   
-----------------------------------------------------------------------------
-- De Morgan

ruleDeMorganOr, ruleDeMorganAnd :: Rule Logic 

ruleDeMorganOr = rule "DeMorganOr" $
   \x y -> (Not (x :||: y))  :~>  (Not x :&&: Not y)

ruleDeMorganAnd = rule "DeMorganAnd" $
   \x y -> (Not (x :&&: y))  :~>  (Not x :||: Not y)
   
-----------------------------------------------------------------------------
-- Implication elimination

ruleDefImpl :: Rule Logic 
ruleDefImpl = rule "DefImpl" $
   \x y -> (x :->: y)  :~>  (Not x :||: y)
   
-----------------------------------------------------------------------------
-- Equivalence elimination

ruleDefEquiv :: Rule Logic 
ruleDefEquiv = rule "DefEquiv" $
   \x y -> (x :<->: y)  :~>  ((x :&&: y) :||: (Not x :&&: Not y))

-----------------------------------------------------------------------------
-- Additional rules, not in the DWA course

groupAdditional :: [Rule Logic]
groupAdditional = makeGroup "Additional rules"
   [ ruleFalseInEquiv, ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
   , ruleCommEquiv, ruleDefEquivImpls, ruleEquivSame, ruleImplSame
   ]

ruleFalseInEquiv :: Rule Logic 
ruleFalseInEquiv = ruleList "FalseInEquiv"
   [ \x -> (F :<->: x)  :~>  (Not x)
   , \x -> (x :<->: F)  :~>  (Not x)
   ]
   
ruleTrueInEquiv :: Rule Logic 
ruleTrueInEquiv = ruleList "TrueInEquiv"
   [ \x -> (T :<->: x)  :~>  x
   , \x -> (x :<->: T)  :~>  x
   ]

ruleFalseInImpl :: Rule Logic 
ruleFalseInImpl = ruleList "FalseInImpl"
   [ \x -> (F :->: x)  :~>  T
   , \x -> (x :->: F)  :~> (Not x)
   ]
   
ruleTrueInImpl :: Rule Logic 
ruleTrueInImpl = ruleList "TrueInImpl"
   [ \x -> (T :->: x)  :~>  x
   , \x -> (x :->: T)  :~>  T
   ]
        
ruleCommEquiv :: Rule Logic 
ruleCommEquiv = rule "CommEquiv" $
   \x y -> (x :<->: y)  :~>  (y :<->: x)

ruleDefEquivImpls :: Rule Logic 
ruleDefEquivImpls = rule "DefEquivImpls" $
   \x y -> (x :<->: y)  :~>  ((x :->: y) :&&: (y :->: x))

ruleEquivSame :: Rule Logic 
ruleEquivSame = rule "EquivSame" $
   \x -> (x :<->: x)  :~>  T

ruleImplSame :: Rule Logic 
ruleImplSame = rule "ImplSame" $
   \x -> (x :->: x)  :~>  T
   
-----------------------------------------------------------------------------
-- Buggy rules

buggyRuleAndSame :: Rule Logic
buggyRuleAndSame = buggyRule $ rule "AndSame" $
   \x -> (x :&&: x)  :~>  T

buggyRuleAndCompl :: Rule Logic
buggyRuleAndCompl = buggyRule $ ruleList "AndComplBuggy"
   [ \x -> (x :&&: Not x)  :~>  T
   , \x -> (Not x :&&: x)  :~>  T
   , \x -> (x :&&: Not x)  :~>  x
   , \x -> (Not x :&&: x)  :~>  x
   ]
   
buggyRuleOrSame :: Rule Logic
buggyRuleOrSame = buggyRule $ rule "OrSame" $
   \x -> (x :||: x)  :~>  T

buggyRuleOrCompl :: Rule Logic
buggyRuleOrCompl = buggyRule $ ruleList "OrComplBuggy"
   [ \x -> (x :||: Not x)  :~>  F
   , \x -> (Not x :||:  x) :~>  F
   , \x -> (x :||: Not x)  :~>  x
   , \x -> (Not x :||:  x) :~>  x
   ]
    
buggyRuleTrueProp :: Rule Logic
buggyRuleTrueProp = buggyRule $ ruleList "TrueProp" 
   [ \x -> (x :||: T)  :~>  x
   , \x -> (T :||: x)  :~>  x
   , \x -> (x :&&: T)  :~>  T
   , \x -> (T :&&: x)  :~>  T
   ]

buggyRuleFalseProp :: Rule Logic
buggyRuleFalseProp = buggyRule $ ruleList "FalseProp" 
   [ \x -> (x :||: F)  :~>  F
   , \x -> (F :||: x)  :~>  F
   , \x -> (x :&&: F)  :~>  x
   , \x -> (F :&&: x)  :~>  x
   ]

buggyRuleCommImp :: Rule Logic 
buggyRuleCommImp = buggyRule $ rule "CommImp" $
   \x y -> (x :->: y)  :~>  (y :->: x) --this does not hold: T->T => T->x

buggyRuleAssImp :: Rule Logic
buggyRuleAssImp = buggyRule $ ruleList "AssImp"
   [ \x y z -> (x :->: (y :->: z))  :~>  ((x :->: y) :->: z)
   , \x y z -> ((x :->: y) :->: z)  :~>  (x :->: (y :->: z))
   ]
    
buggyRuleIdemImp :: Rule Logic
buggyRuleIdemImp = buggyRule $ rule "IdemImp" $
   \x -> (x :->: x)  :~>  x 
    
buggyRuleIdemEqui :: Rule Logic
buggyRuleIdemEqui = buggyRule $ rule "IdemEqui" $
   \x -> (x :<->: x)  :~>  x 

buggyRuleEquivElim1 :: Rule Logic
buggyRuleEquivElim1 = buggyRule $ ruleList "BuggyEquivElim1"
    [ \x y -> (x :<->: y) :~> ((x :&&: y) :||: Not (x :&&: y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :||: (Not x :&&:  y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :||: ( x :&&: Not y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :||: (x :&&: y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :||: Not (x :||: Not y))
    ]
    
buggyRuleEquivElim2 :: Rule Logic
buggyRuleEquivElim2 = buggyRule $ ruleList "BuggyEquivElim2"
    [\x y -> (x :<->: y) :~> ((x :||: y) :&&: (Not x :||: Not y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :&&: (Not x :&&: Not y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :||: (Not x :||: Not y))
    ]
    
buggyRuleImplElim :: Rule Logic
buggyRuleImplElim = buggyRule $ ruleList "BuggyImplElim" 
   [\x y -> (x :->: y) :~> Not (x :||: y)
   ,\x y -> (x :->: y) :~> (x :||: y)
   ,\x y -> (x :->: y) :~> Not (x :&&: y)
   ]
  
buggyRuleImplElim1 :: Rule Logic
buggyRuleImplElim1 = buggyRule $ rule "BuggyImplElim1"  $  
     \x y -> (x :->: y) :~> (Not x :&&: y)
     
buggyRuleDeMorgan1 :: Rule Logic
buggyRuleDeMorgan1 = buggyRule $ ruleList "BuggyDeMorgan1"
    [ \x y -> (Not (x :&&: y)) :~>  (Not x :||: y)
    , \x y -> (Not (x :&&: y)) :~>  (x :||: Not y)
    , \x y -> (Not (x :&&: y)) :~>  (x :||: y)
    , \x y -> (Not (x :||: y)) :~>  (Not x :&&: y)
    , \x y -> (Not (x :||: y)) :~>  (x :&&: Not y) 
    , \x y -> (Not (x :||: y)) :~>  (x :&&: y)
    ]
    
buggyRuleDeMorgan2 :: Rule Logic
buggyRuleDeMorgan2 = buggyRule $ ruleList "BuggyDeMorgan2"
    [ \x y -> (Not (x :&&: y)) :~>  (Not (Not x :||: Not y))
    , \x y -> (Not (x :||: y)) :~>  (Not (Not x :&&: Not y)) --note the firstNot in both formulas!  
    ]
buggyRuleDeMorgan3 :: Rule Logic    
buggyRuleDeMorgan3 = buggyRule $  rule "BuggyDeMorgan3" $
    \x y -> (Not (x :&&: y)) :~>  (Not x :&&: Not y)

buggyRuleDeMorgan4 :: Rule Logic    
buggyRuleDeMorgan4 = buggyRule $  rule "BuggyDeMorgan4" $   
     \x y -> (Not (x :||: y)) :~>  (Not x :||: Not y)
 
buggyRuleNotOverImpl :: Rule Logic
buggyRuleNotOverImpl = buggyRule $ rule "BuggyNotOverImpl" $
    \x y -> (Not(x :->: y)) :~> (Not x :->: Not y)
    
buggyRuleParenth1 :: Rule Logic
buggyRuleParenth1 = buggyRule $ ruleList "BuggyParenth1"
    [ \x y -> (Not (x :&&: y))     :~> (Not x :&&: y)
    , \x y -> (Not (x :||: y))     :~> (Not x :||: y)
    ]

buggyRuleParenth2 :: Rule Logic
buggyRuleParenth2 = buggyRule $ rule "BuggyParenth2" $
    \x y -> (Not (x :<->: y))    :~> (Not(x :&&: y) :||: (Not x :&&: Not y))
    
buggyRuleParenth3 :: Rule Logic
buggyRuleParenth3 = buggyRule $ ruleList "BuggyParenth3"    
    [ \x y -> (Not(Not x :&&: y))  :~> (x :&&: y) 
    , \x y -> (Not(Not x :||: y))  :~> (x :||: y)
    , \x y -> (Not(Not x :->: y))  :~> (x :->: y)
    , \x y -> (Not(Not x :<->: y)) :~> (x :<->: y)
    ]
   
        
buggyRuleAssoc :: Rule Logic
buggyRuleAssoc = buggyRule $ ruleList "BuggyAssoc"
    [ \x y z -> (x :||: (y :&&: z)) :~> ((x :||: y) :&&: z)
    , \x y z -> ((x :||: y) :&&: z) :~> (x :||: (y :&&: z))
    , \x y z -> ((x :&&: y) :||: z) :~> (x :&&: (y :||: z))
    , \x y z -> (x :&&: (y :||: z)) :~> ((x :&&: y) :||: z)
    ]
    
buggyRuleDistr :: Rule Logic
buggyRuleDistr = buggyRule $ ruleList "BuggyDistr"
   [ \x y z -> (x :&&: (y :||: z))  :~>  ((x :&&: y) :&&: (x :&&: z))
   , \x y z -> ((x :||: y) :&&: z)  :~>  ((x :&&: z) :&&: (y :&&: z))
   , \x y z -> (x :&&: (y :||: z))  :~>  ((x :||: y) :&&: (x :||: z))
   , \x y z -> ((x :||: y) :&&: z)  :~>  ((x :||: z) :&&: (y :||: z))
   , \x y z -> (x :||: (y :&&: z))  :~>  ((x :||: y) :||: (x :||: z))
   , \x y z -> ((x :&&: y) :||: z)  :~>  ((x :||: z) :||: (y :||: z))
   , \x y z -> (x :||: (y :&&: z))  :~>  ((x :&&: y) :||: (x :&&: z))
   , \x y z -> ((x :&&: y) :||: z)  :~>  ((x :&&: z) :||: (y :&&: z))
   ] 