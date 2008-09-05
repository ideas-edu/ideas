-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)e
--
-----------------------------------------------------------------------------
module Domain.Fraction.Strategies where

import Prelude hiding (repeat, fail)
import Domain.Fraction.Frac
import Domain.Fraction.Rules
import Common.Context (Context, liftRuleToContext)
import Common.Strategy
import Common.Transformation

lrtc = liftRuleToContext

toSimple :: LabeledStrategy (Context Frac)
toSimple = label "Simplify fractions" $ repeat $ 
                 (  topDown cleanup               -- first remove units and zeros from top to bottom if possible
                 |> bottomUp (mul <|> div')       -- if clean, then multiply and dived bottom up
                 |> bottomUp (add <|> sub))       -- then add and subtract
             <*> negcon -- after every action, fire the minor rule to prevent (Neg (Con x))

zeroRules, unitRules, calcRules, negRules :: [Rule Frac]
zeroRules = [ruleDivZero, ruleMulZero, ruleUnitAdd, ruleSubZero]
unitRules = [ruleUnitMul, ruleDivOne, ruleDivSame, ruleSubVar]
calcRules = [ruleMul, ruleDivFrac, ruleAdd, ruleSub, ruleGCD, ruleDistMul]
negRules  = [ruleNeg, rulePushNeg]

mul :: LabeledStrategy (Context Frac)
mul = label "Do multiplication" $  lrtc ruleMul <|> lrtc ruleMulFrac
                 <|> lrtc ruleAssMul <*> somewhere (lrtc ruleMul <|> lrtc ruleMulFrac)

div' :: LabeledStrategy (Context Frac)
div' = label "Do division" $ lrtc ruleDivFrac

add :: LabeledStrategy (Context Frac)
add = label "Do addition" $ lrtc ruleAdd <|> addFrac
                 <|> lrtc ruleAssAdd <*> somewhere (lrtc ruleAdd <|> addFrac)

add' = label "Do addition" $ lrtc ruleAdd <|> addFrac
                 <|> (lrtc ruleAssAdd <|> fail) <*> somewhere (lrtc ruleAdd <|> lrtc ruleAddFrac)

sub :: LabeledStrategy (Context Frac)
sub = label "Do subtraction" $ lrtc ruleSub <|> subFrac

cleanup :: LabeledStrategy (Context Frac)
cleanup = label "Remove units and zeros" $ alternatives $ map lrtc $ zeroRules ++ unitRules ++ negRules 

negcon :: Strategy (Context Frac)
negcon = try $ repeat $ somewhere $ lrtc ruleNegToCon

addFrac, subFrac :: Strategy (Context Frac)
addFrac =  option (lrtc ruleCommonDenom) <*> lrtc ruleAddFrac <*> option (lrtc ruleGCD)
subFrac =  option (lrtc ruleCommonDenom) <*> lrtc ruleSubFrac <*> option (lrtc ruleGCD)



