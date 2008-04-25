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
import Common.Context
import Common.Apply
import Domain.Fraction.Parser

lrtc = liftRuleToContext
foldComb comb unit rs = foldr comb unit $ map lrtc rs

toSimple :: LabeledStrategy (Context Frac)
toSimple = label "All rules" $ repeat $ zero <*> unit <*> option calc
  where
    zero = label "Eliminate zeros" $ repeat $ somewhere $ foldComb (<|>) fail zeroRules
    unit = label "Eliminate units" $ repeat $ somewhere $ foldComb (<|>) fail unitRules
    calc = label "Do calculation"  $ somewhere $ (foldComb (<|>) fail calcRules <|> calcFrac')

toSimple'' = label "All rules" $ repeat $ somewhere $ foldComb (<|>) fail negRules
             -- foldComb (<|>) fail $ zeroRules ++ unitRules ++ calcRules ++ gcdRules


toSimple' =  label "Alternative strategy" $ repeat $ somewhere $ calc <|> (asscomm <*> calc)
             where
               calc = cleanup <|> mul <|> div <|> add <|> sub <|> neg
               cleanup = label "Remove units and zeros" $ 
                           foldComb (<|>) fail (zeroRules ++ unitRules)
               mul = label "Do multiplication"  $ foldComb (<|>) fail [ruleMul, ruleMulFrac]
               div = label "Do division"        $ lrtc ruleDivFrac
               add = label "Do addition"        $ lrtc ruleAdd <|> ((option (lrtc ruleCommonDenom)) 
                                                                   <*> lrtc ruleAddFrac
                                                                   <*> (option (lrtc ruleGCD)))
               sub = label "Do subtraction"     $ lrtc ruleSub <|> ((option (lrtc ruleCommonDenom)) 
                                                                   <*> lrtc ruleSubFrac
                                                                   <*> (option (lrtc ruleGCD)))
               neg = label ""                   $ foldComb (<|>) fail negRules
               asscomm = label "" $ foldComb (<|>) fail [ruleAssAdd, ruleCommAdd, ruleAssMul, ruleCommMul]           
                   
zeroRules = [ruleDivZero, ruleMulZero, ruleUnitAdd, ruleSubZero]
unitRules = [ruleUnitMul, ruleDivOne, ruleDivSame, ruleSubVar]
calcRules = [ruleMul, ruleDivFrac, ruleAdd, ruleSub, ruleGCD, ruleDistMul]
negRules  = [ruleNeg, rulePushNeg]

calcFrac :: Strategy (Context Frac)
calcFrac =  lrtc ruleCommonDenom 
        <*> (lrtc ruleAddFrac <|> lrtc ruleSubFrac)
        <*> lrtc ruleGCD

gcdRules = [ruleCommonDenom, ruleAddFrac, ruleSubFrac, ruleGCD]

calcFrac' =  lrtc ruleCommonDenom <*> lrtc ruleAddFrac
         <|> lrtc ruleCommonDenom <*> lrtc ruleSubFrac

