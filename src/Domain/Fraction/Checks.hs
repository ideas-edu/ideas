-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Checks (checks) where

import Common.Strategy
import Common.Transformation
import Common.Utils
import Common.Move
import Domain.Fraction.Frac
import Domain.Fraction.Zipper
import Domain.Fraction.Strategies
import Domain.Fraction.Parser
import Domain.Fraction.Rules
import Test.QuickCheck
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Ratio

-----------------------------------------------------------
--- QuickCheck properties

checks :: IO ()
checks = do
   mapM_ checkRule logicRules
   quickCheck propRuleNames
   thoroughCheck propParsePP
   thoroughCheck propCtxPP
   thoroughCheck propContext
   quickCheck propStratDNF

-- correctness logic rules
checkRule :: LogicRule -> IO ()
checkRule rule = do
   putStrLn $ "[" ++ name rule ++ "]"
   quickCheck (propRuleTopLevel rule)
   quickCheck (propRuleSomewhere rule)
   
propRuleTopLevel :: LogicRule -> Logic -> Property
propRuleTopLevel rule logic =
   applicable rule logic ==>
      logic `eqLogic` applyD rule logic

propRuleSomewhere :: LogicRule -> Logic -> Property
propRuleSomewhere rule logic =
   let strat = somewhere (logicRuleInContext rule) 
       lic   = inContext logic
   in applicable strat lic ==>
         logic `eqLogic` noContext (applyD strat lic)
      
-- all rule names are distinct
propRuleNames :: Bool
propRuleNames = length xs == length (nub xs)
 where xs = map name logicRules

-- zipper
propContext :: Movement -> LogicInContext -> Property      
propContext dir loc = 
   let newloc = move dir loc 
   in isJust newloc ==> noContext loc == noContext (fromJust newloc)
         
-- parser/pretty-printer
propParsePP :: Logic -> Bool
propParsePP p = null msgs && p == q
 where (q, msgs) = parseLogic (ppLogic p)

-- pretty-printer zipper
propCtxPP :: LogicInContext -> Bool
propCtxPP ctx = ppLogic (noContext ctx) ~= ppLogicInContext ctx
 where s ~= t = filter p s == filter p t
       p c = not (c `elem` "[]" || isSpace c)

propStratDNF :: Logic -> Property
propStratDNF logic = 
   let isSimple  = (<= 5) . foldLogic (const 1, binop, binop, binop, binop, succ, 1, 1)
       binop x y = succ (x `max` y)
   in isSimple logic ==>
      case apply toDNF (inContext logic) of
         Just this -> isDNF (noContext this)
         _ -> False
 
qqq = Not (Var "r" :<->: Var "p")
                
-----------------------------------------------------------
--- QuickCheck generators

instance Arbitrary Logic where
   arbitrary = sized arbLogic
   coarbitrary logic = 
      case logic of
         Var x     -> variant 0 . coarbitrary (map ord x)
         p :->: q  -> variant 1 . coarbitrary p . coarbitrary q
         p :<->: q -> variant 2 . coarbitrary p . coarbitrary q
         p :&&: q  -> variant 3 . coarbitrary p . coarbitrary q
         p :||: q  -> variant 4 . coarbitrary p . coarbitrary q
         Not p     -> variant 5 . coarbitrary p
         T         -> variant 6  
         F         -> variant 7 

arbLogic :: Int -> Gen Logic
arbLogic 0 = oneof $ map return [Var "p", Var "q", Var "r", F, T]
arbLogic n = oneof [arbLogic 0, op2 (:->:), op2 (:<->:), op2 (:&&:), op2 (:||:), op1 Not]
 where
   op1 f = liftM  f (arbLogic (n `div` 2))
   op2 f = liftM2 f (arbLogic (n `div` 2)) (arbLogic (n `div` 2))
   
instance Arbitrary Cxt where
   arbitrary = sized arbCtx
   coarbitrary ctx =
      case ctx of
         Top        -> variant 0 
         ImplL c l  -> variant 1 . coarbitrary c . coarbitrary l
         ImplR l c  -> variant 2 . coarbitrary l . coarbitrary c
         EquivL c l -> variant 3 . coarbitrary c . coarbitrary l
         EquivR l c -> variant 4 . coarbitrary l . coarbitrary c
         AndL c l   -> variant 5 . coarbitrary c . coarbitrary l
         AndR l c   -> variant 6 . coarbitrary l . coarbitrary c
         OrL c l    -> variant 7 . coarbitrary c . coarbitrary l
         OrR l c    -> variant 8 . coarbitrary l . coarbitrary c
         NotD c     -> variant 9 . coarbitrary c

arbCtx :: Int -> Gen Cxt
arbCtx 0 = return Top
arbCtx n = oneof [ op2l ImplL, op2r ImplR, op2l EquivL, op2r EquivR, op2l AndL, op2r AndR, op2l OrL, op2r OrR, op1 NotD ]
 where
   op1  f = liftM  f (arbCtx (n `div` 2))
   op2l f = liftM2 f (arbCtx (n `div` 2)) (arbLogic (n `div` 2))
   op2r f = liftM2 f (arbLogic (n `div` 2)) (arbCtx (n `div` 2))
   
instance Arbitrary a => Arbitrary (Loc a) where
   arbitrary = liftM2 Loc arbitrary arbitrary
   coarbitrary (Loc a b) = coarbitrary a . coarbitrary b