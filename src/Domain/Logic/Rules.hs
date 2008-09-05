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

transA :: RewriteRule Logic -> [RewriteRule Logic]
transA r = 
   case rulePair r 1 of
      a@(_ :||: _) :~> b -> 
         [r, (a :||: metaVar 0) |- (b :||: metaVar 0)]
      a@(_ :&&: _) :~> b -> 
         [r, (a :&&: metaVar 0) |- (b :&&: metaVar 0)]
      _ -> [r]

mk s xs = makeRuleList s $ concatMap (map RewriteRule . transA . rewriteRule s) xs

-----------------------------------------------------------------------------


ruleComplOr :: Rule Logic 
ruleComplOr = mk "ComplOr"
   [ \x -> (x :||: Not x)  :~>  T
   , \x -> (Not x :||: x)  :~>  T
   ]
   
ruleComplAnd :: Rule Logic 
ruleComplAnd = mk "ComplAnd"
   [ \x -> (x :&&: Not x)  :~>  F
   , \x -> (Not x :&&: x)  :~>  F
   ]

ruleDefImpl :: Rule Logic 
ruleDefImpl = mk "DefImpl"
   [ \x y -> (x :->: y)  :~>  (Not x :||: y)
   ]
   
ruleDefEquiv :: Rule Logic 
ruleDefEquiv = mk "DefEquiv" 
   [ \x y -> (x :<->: y)  :~>  ((x :&&: y) :||: (Not x :&&: Not y))
   ]
   
ruleFalseInEquiv :: Rule Logic 
ruleFalseInEquiv = mk "FalseInEquiv"
   [ \x -> (F :<->: x)  :~>  (Not x)
   , \x -> (x :<->: F)  :~>  (Not x)
   ]
   
ruleTrueInEquiv :: Rule Logic 
ruleTrueInEquiv = mk "TrueInEquiv"
   [ \x -> (T :<->: x)  :~>  x
   , \x -> (x :<->: T)  :~>  x
   ]

ruleFalseInImpl :: Rule Logic 
ruleFalseInImpl = mk "FalseInImpl"
   [ \x -> (F :->: x)  :~>  T
   , \x -> (x :->: F)  :~> (Not x)
   ]
   
ruleTrueInImpl :: Rule Logic 
ruleTrueInImpl = mk "TrueInImpl"
   [ \x -> (T :->: x)  :~>  x
   , \x -> (x :->: T)  :~>  T
   ]
        
ruleFalseZeroOr :: Rule Logic 
ruleFalseZeroOr = mk "FalseZeroOr"
   [ \x -> (F :||: x)  :~>  x
   , \x -> (x :||: F)  :~>  x
   ]
  
ruleTrueZeroOr :: Rule Logic 
ruleTrueZeroOr = mk "TrueZeroOr"
   [ \x -> (T :||: x)  :~>  T
   , \x -> (x :||: T)  :~>  T
   ]

ruleTrueZeroAnd :: Rule Logic 
ruleTrueZeroAnd = mk "TrueZeroAnd"
   [ \x -> (T :&&: x)  :~>  x
   , \x -> (x :&&: T)  :~>  x
   ] 

ruleFalseZeroAnd :: Rule Logic 
ruleFalseZeroAnd = mk "FalseZeroAnd"
   [ \x -> (F :&&: x)  :~>  F
   , \x -> (x :&&: F)  :~>  F
   ]

ruleDeMorganOr :: Rule Logic 
ruleDeMorganOr = mk "DeMorganOr"
   [ \x y -> (Not (x :||: y))  :~>  (Not x :&&: Not y)
   ]

ruleDeMorganAnd :: Rule Logic 
ruleDeMorganAnd = mk "DeMorganAnd"
   [ \x y -> (Not (x :&&: y))  :~>  (Not x :||: Not y)
   ]

ruleNotBoolConst :: Rule Logic 
ruleNotBoolConst = mk "NotBoolConst"
   [ (Not T)  :~>  F
   , (Not F)  :~>  T
   ]

ruleNotNot :: Rule Logic 
ruleNotNot = mk "NotNot"
   [ \x -> (Not (Not x))  :~>  x
   ]

ruleAndOverOr :: Rule Logic 
ruleAndOverOr = mk "AndOverOr"
   [ \x y z -> (x :&&: (y :||: z))  :~>  ((x :&&: y) :||: (x :&&: z))
   , \x y z -> ((x :||: y) :&&: z)  :~>  ((x :&&: z) :||: (y :&&: z))
   ]

ruleOrOverAnd :: Rule Logic 
ruleOrOverAnd = mk "OrOverAnd"
   [ \x y z -> (x :||: (y :&&: z))  :~>  ((x :||: y) :&&: (x :||: z))
   , \x y z -> ((x :&&: y) :||: z)  :~>  ((x :||: z) :&&: (y :||: z))
   ]
   
ruleIdempOr :: Rule Logic 
ruleIdempOr = mk "IdempOr"
   [ \x -> (x :||: x)  :~>  x
   ]
   
ruleIdempAnd :: Rule Logic 
ruleIdempAnd = mk "IdempAnd"
   [  \x -> (x :&&: x)  :~>  x
   ]
    
ruleAbsorpOr :: Rule Logic 
ruleAbsorpOr = mk "AbsorpOr"
   [ \x  y -> (x :||: (x :&&: y))  :~>  x
   ]
    
    
ruleAbsorpAnd :: Rule Logic 
ruleAbsorpAnd = mk "AbsorpAnd"
   [ \x y -> (x :&&: (x :||: y))  :~>  x 
   ]
    
    
ruleCommOr :: Rule Logic 
ruleCommOr = mk "CommOr"
   [ \x y -> (x :||: y)  :~>  (y :||: x) 
   ]
    
ruleCommAnd :: Rule Logic 
ruleCommAnd = mk "CommAnd" $
   [ \x y -> (x :&&: y)  :~>  (y :&&: x)
   ]
    
-- Buggy rules:

buggyRuleCommImp :: Rule Logic 
buggyRuleCommImp = buggyRule $ mk "CommImp"
   [ \x y -> (x :->: y)  :~>  (y :->: x) --this does not hold: T->T => T->x
   ]

buggyRuleAssImp :: Rule Logic
buggyRuleAssImp = buggyRule $ mk "AssImp"
   [ \x y z -> (x :->: (y :->: z))  :~>  ((x :->: y) :->: z)
   , \x y z -> ((x :->: y) :->: z)  :~>  (x :->: (y :->: z))
   ]
    
buggyRuleIdemImp :: Rule Logic
buggyRuleIdemImp = buggyRule $ mk "IdemImp"
   [ \x -> (x :->: x)  :~>  x 
   ]
    
buggyRuleIdemEqui :: Rule Logic
buggyRuleIdemEqui = buggyRule $ mk "IdemEqui"
   [ \x -> (x :<->: x)  :~>  x 
   ]

buggyRuleEquivElim :: Rule Logic
buggyRuleEquivElim = buggyRule $ mk "BuggyEquivElim"
    [ \x y -> (x :<->: y) :~> ((x :&&: y) :||: Not (x :&&: y))
    , \x y -> (x :<->: y) :~> ((x :||: y) :&&: (Not x :||: Not y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :||: (Not x :&&:  y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :||: ( x :&&: Not y))
    , \x y -> (x :<->: y) :~> ((x :&&: y) :&&: (Not x :&&: Not y))
    ]
    
buggyRuleImplElim :: Rule Logic
buggyRuleImplElim = buggyRule $ mk "BuggyImplElim" $
   [ \x y -> (x :->: y) :~> Not (x :||: y) 
   ]
    
buggyRuleDeMorgan :: Rule Logic
buggyRuleDeMorgan = buggyRule $ mk "BuggyDeMorgan"
    [ \x y -> (Not (x :&&: y)) :~>  (Not x :||: y)
    , \x y -> (Not (x :&&: y)) :~>  (x :||: Not y)
    , \x y -> (Not (x :&&: y)) :~> (Not (Not x :||: Not y))
    , \x y -> (Not (x :||: y)) :~>  (Not x :&&: y)
    , \x y -> (Not (x :||: y)) :~>  (x :&&: Not y)
    , \x y -> (Not (x :||: y)) :~> (Not (Not x :&&: Not y)) --note the firstNot in both formulas!  
    ]

buggyRuleNotOverImpl :: Rule Logic
buggyRuleNotOverImpl = buggyRule $ mk "BuggyNotOverImpl" $
    [ \x y -> (Not(x :->: y)) :~> (Not x :->: Not y)
    ]
    
buggyRuleParenth :: Rule Logic
buggyRuleParenth = buggyRule $ mk "BuggyParenth"
    [ \x y -> (Not (x :&&: y))     :~> (Not x :&&: y)
    , \x y -> (Not (x :||: y))     :~> (Not x :||: y)
    , \x y -> (Not (x :<->: y))    :~> (Not(x :&&: y) :||: (Not x :&&: Not y))
    , \x y -> (Not(Not x :&&: y))  :~> (x :&&: y) 
    , \x y -> (Not(Not x :||: y))  :~> (x :||: y)
    , \x y -> (Not(Not x :->: y))  :~> (x :->: y)
    , \x y -> (Not(Not x :<->: y)) :~> (x :<->: y)
    ]
    
buggyRuleAssoc :: Rule Logic
buggyRuleAssoc = buggyRule $ mk "BuggyAssoc"
    [ \x y z -> (x :||: (y :&&: z)) :~> ((x :||: y) :&&: z)
    , \x y z -> ((x :||: y) :&&: z) :~> (x :||: (y :&&: z))
    , \x y z -> ((x :&&: y) :||: z) :~> (x :&&: (y :||: z))
    , \x y z -> (x :&&: (y :||: z)) :~> ((x :&&: y) :||: z)
    ]