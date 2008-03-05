-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Checks (checks) where

import Common.Exercise
import Common.Strategy
import Common.Transformation
import Common.Utils
import Common.Unification
import Domain.Fraction
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
   mapM_ (checkRule eqFrac) fracRules
   quickCheck propRuleNames
{-   thoroughCheck $ checkParserPretty (==) (f parseFrac) ppFrac
   thoroughCheck $ checkParserPretty eqAssociative (f parseFracPars) ppFracPars
   thoroughCheck $ checkParserPretty eqAssociative (f parseFrac) ppFracPars
   quickCheck propPretty
   thoroughCheck propCtxPP
   thoroughCheck propContext
   quickCheck propStratDNF -}
 where
   f p x | null errs = Right y
         | otherwise = Left (errs, Just y)
    where (y, errs) = p x 
    
-- all rule names are distinct
propRuleNames :: Bool
propRuleNames = length xs == length (nub xs)
 where xs = map name fracRules

{-

-- zipper
propContext :: Movement -> FracInContext -> Property      
propContext dir loc = 
   let newloc = move dir loc 
   in isJust newloc ==> noContext loc == noContext (fromJust newloc)
         
propPretty :: Frac -> Bool
propPretty p = 
   let f pp = filter (`notElem` "()") (pp p)
   in f ppFrac == f ppFracPars
 
-- pretty-printer zipper
propCtxPP :: FracInContext -> Bool
propCtxPP ctx = ppFrac (noContext ctx) ~= ppFracInContext ctx
 where s ~= t = filter p s == filter p t
       p c = not (c `elem` "[]" || isSpace c)

propStratDNF :: Frac -> Property
propStratDNF frac = 
   let isSimple  = (<= 5) . foldFrac (const 1, binop, binop, binop, binop, succ, 1, 1)
       binop x y = succ (x `max` y)
   in isSimple frac ==>
      case apply toDNF (inContext frac) of
         Just this -> isDNF (noContext this)
         _ -> False
     
-}
    
-----------------------------------------------------------
--- QuickCheck generators
   {-
instance Arbitrary Cxt where
   arbitrary = sized arbCtx
   coarbitrary ctx =
      case ctx of
         Top       -> variant 0 
         MulL c l  -> variant 1 . coarbitrary c . coarbitrary l
         MulR l c  -> variant 2 . coarbitrary l . coarbitrary c
         DivL c l  -> variant 3 . coarbitrary c . coarbitrary l
         DivR l c  -> variant 4 . coarbitrary l . coarbitrary c
         AddL c l  -> variant 5 . coarbitrary c . coarbitrary l
         AddR l c  -> variant 6 . coarbitrary l . coarbitrary c
         SubL c l  -> variant 7 . coarbitrary c . coarbitrary l
         SubR l c  -> variant 8 . coarbitrary l . coarbitrary c

arbCtx :: Int -> Gen Cxt
arbCtx 0 = return Top
arbCtx n = oneof [ op2l MulL, op2r MulR, op2l DivL, op2r DivR, op2l AddL
                 , op2r AddR, op2l SubL, op2r SubR
                 ]
 where
--   op1  f = liftM  f (arbCtx (n `div` 2))
   op2l f = liftM2 f (arbCtx (n `div` 2)) arbitrary
   op2r f = liftM2 f arbitrary (arbCtx (n `div` 2))
   
instance Arbitrary a => Arbitrary (Loc a) where
   arbitrary = liftM2 Loc arbitrary arbitrary
   coarbitrary (Loc a b) = coarbitrary a . coarbitrary b
-}