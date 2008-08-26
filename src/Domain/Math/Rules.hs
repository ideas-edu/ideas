module Domain.Math.Rules where

import Common.Context
import Control.Monad
import Data.List
import qualified Data.IntMap as IM
import Domain.Math.Classes
-- import Domain.Math.Condition
import Domain.Math.Expr

data Rule a = R { name :: String, nrOfVars :: Int, rulePair :: Int -> (a, a) }

instance Show (Rule a) where
   show r = "[" ++ name r ++ "]" 

instance Eq (Rule a) where
   r1 == r2 = name r1 == name r2

rule0 :: MetaVar a => String -> (a, a) -> Rule a
rule0 s = R s 0 . const

rule1 :: MetaVar a => String -> (a -> (a, a)) -> Rule a
rule1 s f = R s 1 $ \i -> f (metaVar i)

rule2 :: MetaVar a => String -> (a -> a -> (a, a)) -> Rule a
rule2 s f = R s 2 $ \i -> f (metaVar i) (metaVar (i+1))

rule3 :: MetaVar a => String -> (a -> a -> a -> (a, a)) -> Rule a
rule3 s f = R s 3 $ \i -> f (metaVar i) (metaVar (i+1)) (metaVar (i+2))

rule4 :: MetaVar a => String -> (a -> a -> a -> a -> (a, a)) -> Rule a
rule4 s f = R s 4 $ \i -> f (metaVar i) (metaVar (i+1)) (metaVar (i+2)) (metaVar (i+3))

rule5 :: MetaVar a => String -> (a -> a -> a -> a -> a -> (a, a)) -> Rule a
rule5 s f = R s 5 $ \i -> f (metaVar i) (metaVar (i+1)) (metaVar (i+2)) (metaVar (i+3)) (metaVar (i+4))

inverse :: Rule a -> Rule a
inverse r = r { rulePair = swap . rulePair r }
 where swap (x, y) = (y, x)

{-
ruleCondition :: Rule Expr -> Int -> Condition Expr
ruleCondition r i = c1 /\ c2 /\ c3
 where
   (lhs, rhs) = rulePair r i
   c1 = foldr (:&&:) T [ Atom (WF (metaVar v)) | v <- freeVars lhs \\ freeVars rhs ]
   c2 = foldr (:&&:) T [ Not (Atom (y :==: 0)) | x :/: y <- universe lhs ]
   c3 = foldr (:&&:) T [ Not (Atom (x :<:  0)) | Sqrt x  <- universe lhs ] -}

infixl 1 ~>

(~>) :: a -> a -> (a, a)
lhs ~> rhs = (lhs, rhs)

match :: (MonadPlus m, MetaVar a, UniplateConstr a) => Rule a -> a -> m a
match r e = do 
   let (lhs, rhs) = rulePair r (nextVar e)
   s <- unify lhs e
   if any (`IM.member` s) (freeVars e)
      then mzero
      else return (s |-> rhs)

matchAt :: (MonadPlus m, MetaVar a, UniplateConstr a) => [Int] -> Rule a -> a -> m a
matchAt []     r a = match r a
matchAt (i:is) r a = do 
   new <- matchAt is r y
   return $ f $ xs ++ [new] ++ ys
 where
   (cs, f)    = uniplate a
   (xs, y:ys) = splitAt i cs

-----------------------------------------------------------------------
-- Rule collections

numRules        :: (MetaVar a, Num a)        => [Rule a]
fractionalRules :: (MetaVar a, Fractional a) => [Rule a]
floatingRules   :: (MetaVar a, Floating a)   => [Rule a]
allRules        :: (MetaVar a, Floating a)   => [Rule a]

numRules = 
   [ ruleCommPlus, ruleTransPlus, ruleZeroPlus, ruleZeroPlusComm
   , ruleCommTimes, ruleTransTimes, ruleZeroTimes, ruleZeroTimesComm, ruleOneTimes, ruleOneTimesComm
   , ruleInvNeg, ruleZeroNeg
   , ruleSimplPlusNeg
   , ruleDistrNegPlus, ruleDistrNegTimes, ruleDistrPlusTimes
   ]

fractionalRules = 
   [ ruleZeroDiv, ruleOneDiv
   , ruleSimplDiv, ruleSimplDivTimes
   , ruleDistrTimesDiv, ruleDistrDivNumer, ruleDistrDivDenom
   ]

floatingRules = 
   [ ruleZeroSqrt, ruleOneSqrt
   , ruleSimpleSqrtTimes
   , ruleDistrNegDiv, ruleDistrNegDenom
   , ruleDistrSqrtTimes, ruleDistrSqrtDiv, ruleDistrSqrtDenom
   ]

allRules = numRules ++ fractionalRules ++ floatingRules

-----------------------------------------------------------------------
-- Basic rules Plus

ruleCommPlus, ruleTransPlus, ruleZeroPlus, ruleZeroPlusComm :: (MetaVar a, Num a) => Rule a

ruleCommPlus = rule2 "Comm +" $ \x y -> 
   x+y ~> y+x 
   
ruleTransPlus = rule3 "Trans +" $ \x y z -> 
   (x+y)+z ~> x+(y+z)
   
ruleZeroPlus = rule1 "Zero +" $ \x -> 
   0+x ~> x
   
ruleZeroPlusComm = rule1 "Zero + Comm" $ \x -> 
   x+0 ~> x
   
-----------------------------------------------------------------------
-- Basic rules Times

ruleCommTimes, ruleTransTimes, ruleZeroTimes, ruleZeroTimesComm, ruleOneTimes, ruleOneTimesComm :: (MetaVar a, Num a) => Rule a

ruleCommTimes = rule2 "Comm *" $ \x y -> 
   x*y ~> y*x 
   
ruleTransTimes = rule3 "Trans *" $ \x y z -> 
   (x*y)*z ~> x*(y*z)
   
ruleZeroTimes = rule1 "Zero *" $ \x -> 
   0*x ~> 0

ruleZeroTimesComm = rule1 "Zero * Comm" $ \x -> 
   x*0 ~> 0
   
ruleOneTimes = rule1 "One *" $ \x -> 
   1*x ~> x
   
ruleOneTimesComm = rule1 "One * Comm" $ \x -> 
   x*1 ~> x
   
-----------------------------------------------------------------------
-- Basic rules Negation

ruleInvNeg, ruleZeroNeg :: (MetaVar a, Num a) => Rule a

ruleInvNeg = rule1 "Inv neg" $ \x ->
   -(-x) ~> x
   
ruleZeroNeg = rule3 "Trans neg" $ \x y z -> 
   -0 ~> 0
   
-----------------------------------------------------------------------
-- Basic rules Division

ruleZeroDiv, ruleOneDiv :: (MetaVar a, Fractional a) => Rule a

ruleZeroDiv = rule1 "Zero /" $ \x ->
   0/x ~> 0
   
ruleOneDiv = rule3 "One /" $ \x y z -> 
   x/1 ~> x
   
-----------------------------------------------------------------------
-- Basic rules Square Roots

ruleZeroSqrt, ruleOneSqrt :: (MetaVar a, Floating a) => Rule a

ruleZeroSqrt = rule0 "Zero sqrt" $ 
   sqrt 0 ~> 0
   
ruleOneSqrt = rule0 "One sqrt" $ 
   sqrt 1 ~> 1

-----------------------------------------------------------------------
-- Simplification rules

ruleSimplPlusNeg, ruleSimplPlusNegComm :: (MetaVar a, Num a)        => Rule a
ruleSimplDiv, ruleSimplDivTimes        :: (MetaVar a, Fractional a) => Rule a
ruleSimpleSqrtTimes                    :: (MetaVar a, Floating a)   => Rule a

ruleSimplPlusNeg = rule1 "Simpl + neg" $ \x ->
   -x+x ~> 0

ruleSimplPlusNegComm = rule1 "Simpl + neg Comm" $ \x ->
   x+(-x) ~> 0
   
ruleSimplDiv = rule1 "Simpl /" $ \x -> 
   x/x ~> 1
   
ruleSimplDivTimes = rule3 "Simpl / *" $ \x y z -> 
   (x*y)/(x*z) ~> y/z

ruleSimpleSqrtTimes = rule1 "Simpl sqrt *" $ \x -> 
   sqrt x*sqrt x ~> x

-----------------------------------------------------------------------
-- Distribution rules for Negation

ruleDistrNegPlus, ruleDistrNegTimes :: (MetaVar a, Num a)      => Rule a
ruleDistrNegDiv, ruleDistrNegDenom  :: (MetaVar a, Floating a) => Rule a

ruleDistrNegPlus = rule2 "Distr neg +" $ \x y -> 
   -(x+y) ~> (-x)+(-y)
   
ruleDistrNegTimes = rule2 "Distr neg *" $ \x y -> 
   -(x*y) ~> (-x)*y
   
ruleDistrNegDiv = rule2 "Distr neg /" $ \x y -> 
   -(x/y) ~> (-x)/y
   
ruleDistrNegDenom = rule2 "Distr neg denom" $ \x y -> 
   x/(-y) ~> -(x/y)
   
-----------------------------------------------------------------------
-- Remaining distribution rules

ruleDistrPlusTimes                                       :: (MetaVar a, Num a)        => Rule a
ruleDistrTimesDiv, ruleDistrDivNumer, ruleDistrDivDenom  :: (MetaVar a, Fractional a) => Rule a
ruleDistrSqrtTimes, ruleDistrSqrtDiv, ruleDistrSqrtDenom :: (MetaVar a, Floating a)   => Rule a

ruleDistrPlusTimes = rule3 "Distr + *" $ \x y z -> 
   x*(y+z) ~> (x*y)+(x*z)
   
ruleDistrTimesDiv = rule3 "Distr * /" $ \x y z -> 
   x*(y/z) ~> (x*y)/z
   
ruleDistrDivNumer = rule3 "Distr / numer" $ \x y z -> 
   (x/y)/z ~> x/(y*z)
   
ruleDistrDivDenom = rule3 "Distr / denom" $ \x y z -> 
   x/(y/z) ~> x*(z/y)
   
ruleDistrSqrtTimes = rule2 "Distr sqrt *" $ \x y -> 
   sqrt x * sqrt y ~> sqrt (x*y)
   
ruleDistrSqrtDiv = rule2 "Distr sqrt /" $ \x y -> 
   sqrt (x/y) ~> sqrt x / sqrt y
   
ruleDistrSqrtDenom = rule2 "Distr sqrt denom" $ \x y -> 
   x/sqrt y ~> (x*sqrt y)/y