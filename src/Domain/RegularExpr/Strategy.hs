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
module Domain.RegularExpr.Strategy (deterministicStrategy) where

import Domain.RegularExpr.Expr
import Common.Context
import Common.Strategy
import Common.Rewriting
import Common.Transformation
import Prelude hiding (repeat, replicate)

deterministicStrategy :: LabeledStrategy (Context RegExp)
deterministicStrategy = label "deterministic and precise" $ 
   repeat (somewhere
   ((liftToContext ruleLeftFactor1 <|> 
    liftToContext ruleLeftFactor2 <|> 
    liftToContext ruleIdempOr <|>
   -- liftToContext ruleEpsilonSeq <|>
   -- liftToContext ruleEmptySeq <|>
   -- liftToContext ruleEmptyChoice <|>
    liftToContext ruleDefOption) |>
    liftToContext ruleCommFactor))
    <*> 
    repeat (somewhere (liftToContext ruleIntroOption))

ruleLeftFactor1 :: Rule RegExp
ruleLeftFactor1 = rule "LeftFactor1" $ \a x y -> 
   (a :*: x) :|: (a :*: y)  :~>  a :*: (x :|: y)
   
ruleLeftFactor2 :: Rule RegExp
ruleLeftFactor2 = ruleList "LeftFactor2"
   [ \a x -> (a :*: x) :|: a  :~>  a :*: Option x
   , \a x -> a :|: (a :*: x)  :~>  a :*: Option x
   ]

ruleIdempOr :: Rule RegExp
ruleIdempOr = rule "IdempOr" $ \a -> 
   a :|: a  :~>  a
 
ruleCommFactor :: Rule RegExp
ruleCommFactor = ruleList "CommFactor"
   [ \a b _ _ -> a :|: b :|: a  :~>  a :|: a :|: b
   , \a b x _ -> (a :*: x) :|: b :|: a  :~>  (a :*: x) :|: a :|: b
   , \a b y _ -> a :|: b :|: (a :*: y)  :~>  a :|: (a :*: y) :|: b
   , \a b x y -> (a :*: x) :|: b :|: (a :*: y)  :~>  (a :*: x) :|: (a :*: y) :|: b
   ]
   

   
ruleDefOption :: Rule RegExp
ruleDefOption = rule "DefOption" $ \a ->
   Option a  :~>  Epsilon :|: a
   
ruleIntroOption :: Rule RegExp
ruleIntroOption = ruleList "IntroOption" 
   [ \a -> Epsilon :|: a  :~>  Option a
   , \a -> a :|: Epsilon  :~>  Option a
   ]
   
---
{-
ruleEpsilonSeq :: Rule RegExp
ruleEpsilonSeq = ruleList "EpsilonSeq" 
   [ \a -> Epsilon :*: a  :~>  a
   , \a -> a :*: Epsilon  :~> a
   ]
   
ruleEmptySeq :: Rule RegExp
ruleEmptySeq = ruleList "EmptySeq" 
   [ \a -> EmptySet :*: a  :~> EmptySet
   , \a -> a :*: EmptySet  :~> EmptySet
   ]
   
ruleEmptyChoice :: Rule RegExp
ruleEmptyChoice = ruleList "EmptyChoice" 
   [ \a -> EmptySet :|: a  :~> a
   , \a -> a :|: EmptySet  :~> a
   ]
-} 
-----------------
{-
ruleComm :: Rule RegExp
ruleComm = makeSimpleRuleList "Comm" $ \re -> do
   let xs = collectWithOperator choiceOp re
   i <- [0..length xs-1]
   j <- [i+2..length xs-1]
   let (as, b:bs) = splitAt i xs
       (cs, d:ds) = splitAt (j-i-1) bs
   guard (all (`notElem` (lookahead b)) (lookahead d))
   let new = as++[b,d]++cs++ds
   return (buildWithOperator choiceOp new)-}