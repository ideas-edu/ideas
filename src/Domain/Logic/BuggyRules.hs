-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
module Domain.Logic.BuggyRules (buggyRules) where

import Common.Library hiding (ruleList)
import qualified Common.Library as C
import Domain.Logic.Formula
import Domain.Logic.Generator()

-- Collection of all known buggy rules
buggyRules :: [Rule SLogic]
buggyRules =
   [ buggyCommImp, buggyAssImp, buggyIdemImp, buggyIdemEqui
   , buggyEquivElim1, buggyImplElim2, buggyEquivElim2, buggyEquivElim3
   , buggyImplElim, buggyImplElim1, buggyDeMorgan1, buggyDeMorgan2, buggyDeMorgan3
   , buggyDeMorgan4, buggyDeMorgan5, buggyNotOverImpl, buggyParenth1, buggyParenth2
   , buggyParenth3, buggyAssoc, buggyAbsor
   , buggyAndSame, buggyAndCompl, buggyOrSame, buggyOrCompl
   , buggyTrueProp, buggyFalseProp, buggyDistr, buggyDistrNot
   ]

rule :: RuleBuilder f a => String -> f -> Rule a
rule = C.rewriteRule . ( "logic.propositional.buggy" # )

ruleList :: RuleBuilder f a => String -> [f] -> Rule a
ruleList = C.rewriteRules . ( "logic.propositional.buggy" # )

-----------------------------------------------------------------------------
-- Buggy rules

buggyAndSame :: Rule SLogic
buggyAndSame = buggy $ rule "AndSame" $
   \x -> x :&&: x  :~>  T

buggyAndCompl :: Rule SLogic
buggyAndCompl = buggy $ ruleList "AndCompl"
   [ \x -> x :&&: Not x  :~>  T
   , \x -> Not x :&&: x  :~>  T
   , \x -> x :&&: Not x  :~>  x
   , \x -> Not x :&&: x  :~>  x
   ]

buggyOrSame :: Rule SLogic
buggyOrSame = buggy $ rule "OrSame" $
   \x -> x :||: x  :~>  T

buggyOrCompl :: Rule SLogic
buggyOrCompl = buggy $ ruleList "OrCompl"
   [ \x -> x :||: Not x  :~>  F
   , \x -> Not x :||:  x :~>  F
   , \x -> x :||: Not x  :~>  x
   , \x -> Not x :||:  x :~>  x
   ]

buggyTrueProp :: Rule SLogic
buggyTrueProp = buggy $ ruleList "TrueProp"
   [ \x -> x :||: T  :~>  x
   , \x -> T :||: x  :~>  x
   , \x -> x :&&: T  :~>  T
   , \x -> T :&&: x  :~>  T
   ]

buggyFalseProp :: Rule SLogic
buggyFalseProp = buggy $ ruleList "FalseProp"
   [ \x -> x :||: F  :~>  F
   , \x -> F :||: x  :~>  F
   , \x -> x :&&: F  :~>  x
   , \x -> F :&&: x  :~>  x
   ]

buggyCommImp :: Rule SLogic
buggyCommImp = buggy $ rule "CommImp" $
   \x y -> x :->: y  :~>  y :->: x --this does not hold: T->T => T->x

buggyAssImp :: Rule SLogic
buggyAssImp = buggy $ ruleList "AssImp"
   [ \x y z -> x :->: (y :->: z)  :~>  (x :->: y) :->: z
   , \x y z -> (x :->: y) :->: z  :~>  x :->: (y :->: z)
   ]

buggyIdemImp :: Rule SLogic
buggyIdemImp = buggy $ rule "IdemImp" $
   \x -> x :->: x  :~>  x

buggyIdemEqui :: Rule SLogic
buggyIdemEqui = buggy $ rule "IdemEqui" $
   \x -> x :<->: x  :~>  x

buggyEquivElim1 :: Rule SLogic
buggyEquivElim1 = buggy $ ruleList "EquivElim1"
    [ \x y -> x :<->: y :~> (x :&&: y) :||: Not (x :&&: y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: (Not x :&&:  y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: ( x :&&: Not y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: (x :&&: y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: Not (x :||: Not y)
    ]

buggyEquivElim2 :: Rule SLogic
buggyEquivElim2 = buggy $ ruleList "EquivElim2"
    [ \x y -> x :<->: y :~> (x :||: y) :&&: (Not x :||: Not y)
    , \x y -> x :<->: y :~> (x :&&: y) :&&: (Not x :&&: Not y)
    , \x y -> x :<->: y :~> (x :&&: y) :||: (Not x :||: Not y)
    ]

buggyEquivElim3 :: Rule SLogic
buggyEquivElim3 = buggy $ rule "EquivElim3"  $
     \x y -> x :<->: y :~> Not x :||: y

buggyImplElim :: Rule SLogic
buggyImplElim = buggy $ ruleList "ImplElim"
   [\x y -> x :->: y :~> Not (x :||: y)
   ,\x y -> x :->: y :~> (x :||: y)
   ,\x y -> x :->: y :~> Not (x :&&: y)
   ]

buggyImplElim1 :: Rule SLogic
buggyImplElim1 = buggy $ rule "ImplElim1"  $
     \x y -> x :->: y :~> Not x :&&: y

buggyImplElim2 :: Rule SLogic
buggyImplElim2 = buggy $ rule "ImplElim2" $
     \x y -> x :->: y :~>  (x :&&: y) :||: (Not x :&&: Not y)

buggyDeMorgan1 :: Rule SLogic
buggyDeMorgan1 = buggy $ ruleList "DeMorgan1"
    [ \x y -> Not (x :&&: y) :~>  Not x :||: y
    , \x y -> Not (x :&&: y) :~>  x :||: Not y
    , \x y -> Not (x :&&: y) :~>  x :||: y
    , \x y -> Not (x :||: y) :~>  Not x :&&: y
    , \x y -> Not (x :||: y) :~>  x :&&: Not y
    , \x y -> Not (x :||: y) :~>  x :&&: y
    ]

buggyDeMorgan2 :: Rule SLogic
buggyDeMorgan2 = buggy $ ruleList "DeMorgan2"
    [ \x y -> Not (x :&&: y) :~>  Not (Not x :||: Not y)
    , \x y -> Not (x :||: y) :~>  Not (Not x :&&: Not y) --note the firstNot in both formulas!
    ]
buggyDeMorgan3 :: Rule SLogic
buggyDeMorgan3 = buggy $  rule "DeMorgan3" $
    \x y -> Not (x :&&: y) :~>  Not x :&&: Not y

buggyDeMorgan4 :: Rule SLogic
buggyDeMorgan4 = buggy $  rule "DeMorgan4" $
     \x y -> Not (x :||: y) :~>  Not x :||: Not y

buggyDeMorgan5 :: Rule SLogic
buggyDeMorgan5 = buggy $ ruleList "DeMorgan5"
    [ \x y z -> Not (Not (x :&&: y) :||: z) :~>  Not (Not x :||: Not y):||: z
    , \x y z -> Not (Not (x :&&: y) :&&: z) :~>  Not (Not x :||: Not y):&&: z
    , \x y z -> Not (Not (x :||: y) :||: z) :~>  Not (Not x :&&: Not y):||: z
    , \x y z -> Not (Not (x :||: y) :&&: z) :~>  Not (Not x :&&: Not y):&&: z
    ]

buggyNotOverImpl :: Rule SLogic
buggyNotOverImpl = buggy $ rule "NotOverImpl" $
    \x y -> Not (x :->: y) :~> Not x :->: Not y

buggyParenth1 :: Rule SLogic
buggyParenth1 = buggy $ ruleList "Parenth1"
    [ \x y -> Not (x :&&: y)     :~> Not x :&&: y
    , \x y -> Not (x :||: y)     :~> Not x :||: y
    ]

buggyParenth2 :: Rule SLogic
buggyParenth2 = buggy $ rule "Parenth2" $
    \x y -> Not (x :<->: y) :~> Not(x :&&: y) :||: (Not x :&&: Not y)

buggyParenth3 :: Rule SLogic
buggyParenth3 = buggy $ ruleList "Parenth3"
    [ \x y -> Not (Not x :&&: y)  :~> x :&&: y
    , \x y -> Not (Not x :||: y)  :~> x :||: y
    , \x y -> Not (Not x :->: y)  :~> x :->: y
    , \x y -> Not (Not x :<->: y) :~> x :<->: y
    ]

buggyAssoc :: Rule SLogic
buggyAssoc = buggy $ ruleList "Assoc"
    [ \x y z -> x :||: (y :&&: z) :~> (x :||: y) :&&: z
    , \x y z -> (x :||: y) :&&: z :~> x :||: (y :&&: z)
    , \x y z -> (x :&&: y) :||: z :~> x :&&: (y :||: z)
    , \x y z -> x :&&: (y :||: z) :~> (x :&&: y) :||: z
    ]

buggyAbsor :: Rule SLogic
buggyAbsor = buggy $ ruleList "Absor"
    [ \x y z -> (x :||: y) :||: ((x :&&: y) :&&: z) :~> (x :||: y)
    , \x y z -> (x :&&: y) :||: ((x :||: y) :&&: z) :~> (x :&&: y)
    , \x y z -> (x :||: y) :&&: ((x :&&: y) :||: z) :~> (x :||: y)
    , \x y z -> (x :&&: y) :&&: ((x :||: y) :||: z) :~> (x :&&: y)
    ]

buggyDistr :: Rule SLogic
buggyDistr = buggy $ ruleList "Distr"
   [ \x y z -> x :&&: (y :||: z)  :~>  (x :&&: y) :&&: (x :&&: z)
   , \x y z -> (x :||: y) :&&: z  :~>  (x :&&: z) :&&: (y :&&: z)
   , \x y z -> x :&&: (y :||: z)  :~>  (x :||: y) :&&: (x :||: z)
   , \x y z -> (x :||: y) :&&: z  :~>  (x :||: z) :&&: (y :||: z)
   , \x y z -> x :||: (y :&&: z)  :~>  (x :||: y) :||: (x :||: z)
   , \x y z -> (x :&&: y) :||: z  :~>  (x :||: z) :||: (y :||: z)
   , \x y z -> x :||: (y :&&: z)  :~>  (x :&&: y) :||: (x :&&: z)
   , \x y z -> (x :&&: y) :||: z  :~>  (x :&&: z) :||: (y :&&: z)
   ]

buggyDistrNot :: Rule SLogic
buggyDistrNot = buggy $ ruleList "DistrNot"
   [ \x y z -> Not x :&&: (y :||: z)  :~>  (Not x :&&: y) :||: (x :&&: z)
   , \x y z -> Not x :&&: (y :||: z)  :~>  (x :&&: y) :||: (Not x :&&: z)
   , \x y z -> (x :||: y) :&&: Not z  :~>  (x :&&: Not z) :||: (y :&&: z)
   , \x y z -> (x :||: y) :&&: Not z  :~>  (x :&&: z) :||: (y :&&: Not z)
   , \x y z -> Not x :||: (y :&&: z)  :~>  (Not x :||: y) :&&: (x :||: z)
   , \x y z -> Not x :||: (y :&&: z)  :~>  (x :||: y) :&&: (Not x :||: z)
   , \x y z -> (x :&&: y) :||: Not z  :~>  (x :||: Not z) :&&: (y :||: z)
   , \x y z -> (x :&&: y) :||: Not z  :~>  (x :||: z) :&&: (y :||: Not z)
   ]