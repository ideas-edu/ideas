module Domain.Math.SExpr (SExpr, toExpr, simplifyExpr, hasSquareRoot) where

import Common.Utils (safeHead)
import Common.Context
import Domain.Math.Classes
import Domain.Math.Expr
import Domain.Math.Rules
import Control.Monad
import Data.Maybe
import Test.QuickCheck

newtype SExpr = SExpr Expr deriving Eq

instance Show SExpr where
   show = show . toExpr

instance Num SExpr where
   (+) = liftS2 (+)
   (*) = liftS2 (*)
   (-) = liftS2 (-)
   negate      = liftS negate
   fromInteger = simplifyExpr . fromInteger

instance Fractional SExpr where
   (/) = liftS2 (/)
   fromRational = simplifyExpr . fromRational
   
instance Floating SExpr where
   sqrt = liftS sqrt
   pi   = simplifyExpr pi
   
instance Symbolic SExpr where
   variable = simplifyExpr . variable
   function s = simplifyExpr . function s . map toExpr
   
instance Arbitrary SExpr where
   arbitrary   = liftM simplifyExpr arbitrary
   coarbitrary = coarbitrary . toExpr
   
toExpr :: SExpr -> Expr
toExpr (SExpr e) = e

liftS  f a   = simplifyExpr $ f (toExpr a)
liftS2 f a b = simplifyExpr $ f (toExpr a) (toExpr b)

-----------------------------------------------------------------------
-- Simplifications

simplifyExpr :: Expr -> SExpr
simplifyExpr = SExpr . fixpoint (transformBU f)
 where 
   f = applyRules . simplifySquareRoots . constantPropagation
            
constantPropagation :: Expr -> Expr
constantPropagation e =
   maybe e fromRational (exprToFractional e)

simplifySquareRoots :: Expr -> Expr
simplifySquareRoots e =
   case e of
      Sqrt (Con a) -> maybe e fromInteger (hasSquareRoot a)
      _ -> e

hasSquareRoot :: Integer -> Maybe Integer
hasSquareRoot n
   | r*r == n  = Just r
   | otherwise = Nothing
 where
   r = round $ sqrt $ fromIntegral n
 
applyRules :: Expr -> Expr
applyRules e = 
   fromMaybe e $ safeHead [ a | r <- rs, a <- match r e ]
 where
   rs = [ rule2 "Def. minus" $ \x y -> x-y ~> x+(-y)
        , ruleZeroPlus, ruleZeroPlusComm 
        , ruleZeroTimes, ruleZeroTimesComm, ruleOneTimes, ruleOneTimesComm
        , ruleInvNeg, ruleZeroNeg
        , ruleZeroDiv, ruleOneDiv
        , ruleSimplPlusNeg, ruleSimplPlusNegComm
        , ruleSimplDiv, ruleSimpleSqrtTimes
        , rule1 "Temp1" $ \x -> x * (1/x) ~> 1
        , rule1 "Temp1" $ \x -> (1/x) * x ~> 1
        ]
   
transformBU :: Uniplate a => (a -> a) -> a -> a
transformBU g a = g $ f $ map (transformBU g) cs
 where
   (cs, f) = uniplate a
   
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = stop . iterate f 
 where
   stop (x:xs) 
      | x == head xs = x
      | otherwise    = stop xs