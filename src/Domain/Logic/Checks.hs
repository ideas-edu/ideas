{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Logic.Checks (checks) where

import Common.Apply
import Common.Exercise
import Common.Context
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Utils
import Common.Parsing
import Domain.Logic
import Common.Unification
import Test.QuickCheck
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

-----------------------------------------------------------
--- QuickCheck properties

-- some temporary ad-hoc rules to test behaviour of bottomUp combinator
{- 
w = runStrategyRules (bottomUp r) prop
prop = inContext (Not (Not (Not (Var "p"))))
r =  liftLogicRule ruleNotNot
fr = fix $ \this -> once this <|> (S.not (once fr) <*> r)
fr1 = once (once (once r |> r) |> r) |> r
fr2 = once fr |> r
re = noLabels fr -}

checks :: IO ()
checks = do
   mapM_ (checkRule eqLogic) logicRules
   quickCheck propRuleNames
   thoroughCheck $ checkParserPretty (==) (f parseLogic) ppLogic
   thoroughCheck $ checkParserPretty eqAssociative (f parseLogicPars) ppLogicPars
   thoroughCheck $ checkParserPretty eqAssociative (f parseLogic) ppLogicPars
   quickCheck propPretty
   -- thoroughCheck propCtxPP
   -- thoroughCheck propContext
   quickCheck propStratDNF
 where
   f p x | null errs = Right (fromRanged y)
         | otherwise = Left (errs, Just y)
    where (y, errs) = p x 
    
-- all rule names are distinct
propRuleNames :: Bool
propRuleNames = length xs == length (nub xs)
 where xs = map name logicRules

-- zipper
{-
propContext :: Movement -> LogicInContext -> Property      
propContext dir loc = 
   let newloc = move dir loc 
   in isJust newloc ==> noContext loc == noContext (fromJust newloc) -}
         
propPretty :: Logic -> Bool
propPretty p = 
   let f pp = filter (`notElem` "()") (pp p)
   in f ppLogic == f ppLogicPars
 
-- pretty-printer zipper
{-
propCtxPP :: LogicInContext -> Bool
propCtxPP ctx = ppLogic (noContext ctx) ~= ppLogicInContext ctx
 where s ~= t = filter p s == filter p t
       p c = not (c `elem` "[]" || isSpace c) -}

propStratDNF :: Logic -> Property
propStratDNF logic = 
   let isSimple  = (<= 5) . foldLogic (const 1, binop, binop, binop, binop, succ, 1, 1)
       binop x y = succ (x `max` y)
   in isSimple logic ==>
      case apply toDNF (inContext logic) of
         Just this -> isDNF (fromContext this)
         _ -> False

propPrefix :: Logic -> Bool
propPrefix logic = isDNF $ fromContext $ rec (inContext logic) emptyPrefix
 where
   stopC (Major _ _) = True
   stopC _           = False
   rec c p = case continuePrefixUntil stopC p c toDNF of
                (d, q):_ | p /= q -> rec d q
                _ -> c

{-
(t1, p1):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p0 t0 toDNF
(t2, p2):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p1 t1 toDNF
(t3, p3):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p2 t2 toDNF
(t4, p4):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p3 t3 toDNF
(t5, p5):_   = continuePrefixUntil (\_ r -> not $ isMinorRule r) p4 t4 toDNF

--t0 = inContext $ Not (Var "x" :||: Var "y") 
t0 = inContext $ Var "p" :<->: Var "q" :||: Var "p"
p0 = emptyPrefix  -}
    
-----------------------------------------------------------
--- QuickCheck generator
{-
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
   op2l f = liftM2 f (arbCtx (n `div` 2)) arbitrary
   op2r f = liftM2 f arbitrary (arbCtx (n `div` 2))
   
instance Arbitrary a => Arbitrary (Loc a) where
   arbitrary = liftM2 Loc arbitrary arbitrary
   coarbitrary (Loc a b) = coarbitrary a . coarbitrary b -}