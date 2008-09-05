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
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.Logic.Rules where

import Domain.Logic.Formula
import Common.Transformation
import Common.Rewriting

-- qqq = confluence $ [ r | RewriteRule r <- concatMap transformations logicRules ]
 
logicRules :: [Rule Logic]
logicRules = 
   [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd, ruleFalseZeroAnd, ruleDeMorganOr, ruleDeMorganAnd
   , ruleNotBoolConst, ruleNotNot, ruleAndOverOr, ruleOrOverAnd
   , ruleDefImpl, ruleDefEquiv
   , ruleFalseInEquiv, ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
   , ruleComplOr, ruleComplAnd
   , ruleIdempOr, ruleIdempAnd
   , ruleAbsorpOr, ruleAbsorpAnd
   , ruleCommOr, ruleCommAnd
   ]

logicBuggyRules :: [Rule Logic]
logicBuggyRules = 
   [ buggyRuleCommImp, buggyRuleAssImp
   ]

-----------------------------------------------------------------------------

ruleComplOr :: Rule Logic 
ruleComplOr = ruleList "ComplOr" $
   [ \x -> (x :||: Not x)  :~>  T
   , \x -> (Not x :||: x)  :~>  T
   ]
   
ruleComplAnd :: Rule Logic 
ruleComplAnd = ruleList "ComplAnd"
   [ \x -> (x :&&: Not x)  :~>  F
   , \x -> (Not x :&&: x)  :~>  F
   ]

ruleDefImpl :: Rule Logic 
ruleDefImpl = rule "DefImpl" $
   \x y -> (x :->: y)  :~>  (Not x :||: y)
   
ruleDefEquiv :: Rule Logic 
ruleDefEquiv = rule "DefEquiv" $
   \x y -> (x :<->: y)  :~>  ((x :&&: y) :||: (Not x :&&: Not y))
   
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
        
ruleFalseZeroOr :: Rule Logic 
ruleFalseZeroOr = ruleList "FalseZeroOr"
   [ \x -> (F :||: x)  :~>  x
   , \x -> (x :||: F)  :~>  x
   ]
  
ruleTrueZeroOr :: Rule Logic 
ruleTrueZeroOr = ruleList "TrueZeroOr"
   [ \x -> (T :||: x)  :~>  T
   , \x -> (x :||: T)  :~>  T
   ]

ruleTrueZeroAnd :: Rule Logic 
ruleTrueZeroAnd = ruleList "TrueZeroAnd"
   [ \x -> (T :&&: x)  :~>  x
   , \x -> (x :&&: T)  :~>  x
   ] 

ruleFalseZeroAnd :: Rule Logic 
ruleFalseZeroAnd = ruleList "FalseZeroAnd"
   [ \x -> (F :&&: x)  :~>  F
   , \x -> (x :&&: F)  :~>  F
   ]

ruleDeMorganOr :: Rule Logic 
ruleDeMorganOr = rule "DeMorganOr" $
   \x y -> (Not (x :||: y))  :~>  (Not x :&&: Not y)

ruleDeMorganAnd :: Rule Logic 
ruleDeMorganAnd = rule "DeMorganAnd" $
   \x y -> (Not (x :&&: y))  :~>  (Not x :||: Not y)

ruleNotBoolConst :: Rule Logic 
ruleNotBoolConst = ruleList "NotBoolConst"
   [ (Not T)  :~>  F
   , (Not F)  :~>  T
   ]

ruleNotNot :: Rule Logic 
ruleNotNot = rule "NotNot" $
   \x -> (Not (Not x))  :~>  x

ruleAndOverOr :: Rule Logic 
ruleAndOverOr = ruleList "AndOverOr"
   [ \x y z -> (x :&&: (y :||: z))  :~>  ((x :&&: y) :||: (x :&&: z))
   , \x y z -> ((x :||: y) :&&: z)  :~>  ((x :&&: z) :||: (y :&&: z))
   ]

ruleOrOverAnd :: Rule Logic 
ruleOrOverAnd = ruleList "OrOverAnd"
   [ \x y z -> (x :||: (y :&&: z))  :~>  ((x :||: y) :&&: (x :||: z))
   , \x y z -> ((x :&&: y) :||: z)  :~>  ((x :||: z) :&&: (y :||: z))
   ]
   
ruleIdempOr :: Rule Logic 
ruleIdempOr = rule "IdempOr" $
   \x -> (x :||: x)  :~>  x
   
ruleIdempAnd :: Rule Logic 
ruleIdempAnd = rule "IdempAnd" $
   \x -> (x :&&: x)  :~>  x
    
ruleAbsorpOr :: Rule Logic 
ruleAbsorpOr = rule "AbsorpOr" $
   \x y -> (x :||: (x :&&: y))  :~>  x
    
    
ruleAbsorpAnd :: Rule Logic 
ruleAbsorpAnd = rule "AbsorpAnd" $
   \x y -> (x :&&: (x :||: y))  :~>  x 
    
ruleCommOr :: Rule Logic 
ruleCommOr = rule "CommOr" $
   \x y -> (x :||: y)  :~>  (y :||: x) 
    
ruleCommAnd :: Rule Logic 
ruleCommAnd = rule "CommAnd" $
   \x y -> (x :&&: y)  :~>  (y :&&: x)
    
-- Buggy rules:

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

buggyRuleEquivElim :: Rule Logic
buggyRuleEquivElim = buggyRule $ ruleList "BuggyEquivElim"
    [ \x y -> (x :<->: y) :~> ((x :&&: y) :||: Not (x :&&: y))
    , \x y -> (x :<->: y) :~> ((x :||: y) :&&: (Not x :||: Not y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :||: (Not x :&&:  y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :||: ( x :&&: Not y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :&&: (Not x :&&: Not y))
    ]
    
buggyRuleImplElim :: Rule Logic
buggyRuleImplElim = buggyRule $ rule "BuggyImplElim" $
   \x y -> (x :->: y) :~> Not (x :||: y) 
    
buggyRuleDeMorgan :: Rule Logic
buggyRuleDeMorgan = buggyRule $ ruleList "BuggyDeMorgan"
    [ \x y -> (Not (x :&&: y)) :~>  (Not x :||: y)
    , \x y -> (Not (x :&&: y)) :~>  (x :||: Not y)
    , \x y -> (Not (x :&&: y)) :~> (Not (Not x :||: Not y))
    , \x y -> (Not (x :||: y)) :~>  (Not x :&&: y)
    , \x y -> (Not (x :||: y)) :~>  (x :&&: Not y)
    , \x y -> (Not (x :||: y)) :~> (Not (Not x :&&: Not y)) --note the firstNot in both formulas!  
    ]

buggyRuleNotOverImpl :: Rule Logic
buggyRuleNotOverImpl = buggyRule $ rule "BuggyNotOverImpl" $
    \x y -> (Not(x :->: y)) :~> (Not x :->: Not y)
    
buggyRuleParenth :: Rule Logic
buggyRuleParenth = buggyRule $ ruleList "BuggyParenth"
    [ \x y -> (Not (x :&&: y))     :~> (Not x :&&: y)
    , \x y -> (Not (x :||: y))     :~> (Not x :||: y)
    , \x y -> (Not (x :<->: y))    :~> (Not(x :&&: y) :||: (Not x :&&: Not y))
    , \x y -> (Not(Not x :&&: y))  :~> (x :&&: y) 
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