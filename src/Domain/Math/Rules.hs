module Domain.Math.Rules where

import Common.Uniplate
import Common.Rewriting
import Control.Monad
import qualified Data.IntSet as IS
import Data.List
import Data.Monoid
import Data.Maybe
import Domain.Math.Constrained

data Rule a = R { name :: String, nrOfVars :: Int, ruleTriple :: Int -> Triple a }

data Triple a = Triple a a (Prop (Con a))

rulePair :: Rule a -> Int -> (a, a)
rulePair r i = (lhs, rhs)
 where Triple lhs rhs _ = ruleTriple r i

instance Show (Rule a) where
   show r = "[" ++ name r ++ "]" 

instance Eq (Rule a) where
   r1 == r2 = name r1 == name r2

rule0 :: MetaVar b => String -> Triple b -> Rule b
rule0 s = R s 0 . const

rule1 :: MetaVar a => String -> (a -> Triple b) -> Rule b
rule1 s f = R s 1 $ \i -> f (metaVar i)

rule2 :: MetaVar a => String -> (a -> a -> Triple b) -> Rule b
rule2 s f = R s 2 $ \i -> f (metaVar i) (metaVar (i+1))

rule3 :: MetaVar a => String -> (a -> a -> a -> Triple b) -> Rule b
rule3 s f = R s 3 $ \i -> f (metaVar i) (metaVar (i+1)) (metaVar (i+2))

rule4 :: MetaVar a => String -> (a -> a -> a -> a -> Triple b) -> Rule b
rule4 s f = R s 4 $ \i -> f (metaVar i) (metaVar (i+1)) (metaVar (i+2)) (metaVar (i+3))

rule5 :: MetaVar a => String -> (a -> a -> a -> a -> a -> Triple b) -> Rule b
rule5 s f = R s 5 $ \i -> f (metaVar i) (metaVar (i+1)) (metaVar (i+2)) (metaVar (i+3)) (metaVar (i+4))

inverse :: (MetaVar a, Uniplate a) => Rule a -> Maybe (Rule a)
inverse r = if checkScope new then Just new else Nothing
 where 
   swap (Triple x y xs) = Triple y x xs
   new = r { ruleTriple = swap . ruleTriple r }

bothWays :: (MetaVar a, Uniplate a) => [Rule a] -> [Rule a]
bothWays rs = rs ++ catMaybes (map inverse rs)

checkScope :: (MetaVar a, Uniplate a) => Rule a -> Bool
checkScope r = IS.null (getMetaVars rhs IS.\\ getMetaVars lhs)
 where (lhs, rhs) = rulePair r 0

infixl 1 ~>, .#.

(~>) :: a -> a -> Triple a
lhs ~> rhs = Triple lhs rhs mzero

(.#.) :: Triple a -> Prop (Con a) -> Triple a
Triple lhs rhs p1 .#. p2 = Triple lhs rhs (p1 `mappend` p2)

-----------------------------------------------------------------------
-- Rule collections

numRules        :: (MetaVar a, Num a)        => [Rule a]
fractionalRules :: (MetaVar a, Fractional a) => [Rule a]
floatingRules   :: (MetaVar a, Floating a)   => [Rule a]
allRules        :: (MetaVar a, Floating a)   => [Rule a]

numRules = 
   [ ruleCommPlus, ruleAssocPlus, ruleZeroPlus, ruleZeroPlusComm
   , ruleCommTimes, ruleAssocTimes, ruleZeroTimes, ruleZeroTimesComm, ruleOneTimes, ruleOneTimesComm
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
   , ruleDistrSqrtTimes, {-ruleDistrSqrtDiv,-} ruleDistrSqrtDenom
   ]

allRules = numRules ++ fractionalRules ++ floatingRules

-----------------------------------------------------------------------
-- Basic rules Plus

ruleCommPlus, ruleAssocPlus, ruleZeroPlus, ruleZeroPlusComm :: (MetaVar a, Num a) => Rule a

ruleCommPlus = rule2 "Comm +" $ \x y -> 
   x+y ~> y+x 
   
ruleAssocPlus = rule3 "Assoc +" $ \x y z -> 
   (x+y)+z ~> x+(y+z)
   
ruleZeroPlus = rule1 "Zero +" $ \x -> 
   0+x ~> x
   
ruleZeroPlusComm = rule1 "Zero + Comm" $ \x -> 
   x+0 ~> x
   
-----------------------------------------------------------------------
-- Basic rules Times

ruleCommTimes, ruleAssocTimes, ruleZeroTimes, ruleZeroTimesComm, ruleOneTimes, ruleOneTimesComm :: (MetaVar a, Num a) => Rule a

ruleCommTimes = rule2 "Comm *" $ \x y -> 
   x*y ~> y*x 
   
ruleAssocTimes = rule3 "Trans *" $ \x y z -> 
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
   
ruleZeroNeg = rule0 "Zero neg" $ 
   -0 ~> 0
   
-----------------------------------------------------------------------
-- Basic rules Division

ruleZeroDiv, ruleOneDiv :: (MetaVar a, Fractional a) => Rule a

ruleZeroDiv = rule1 "Zero /" $ \x ->
   0/x ~> 0   .#. x./=0
   
ruleOneDiv = rule1 "One /" $ \x -> 
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
   x/x ~> 1   .#. x./=0
   
ruleSimplDivTimes = rule3 "Simpl / *" $ \x y z -> 
   (x*y)/(x*z) ~> y/z   .#. x./=0

ruleSimpleSqrtTimes = rule1 "Simpl sqrt *" $ \x -> 
   sqrt x*sqrt x ~> x   .#. x.>=0

-----------------------------------------------------------------------
-- Distribution rules for Negation

ruleDistrNegPlus, ruleDistrNegTimes :: (MetaVar a, Num a)      => Rule a
ruleDistrNegDiv, ruleDistrNegDenom  :: (MetaVar a, Floating a) => Rule a

ruleDistrNegPlus = rule2 "Distr neg +" $ \x y -> 
   -(x+y) ~> (-x)+(-y)
   
ruleDistrNegTimes = rule2 "Distr neg *" $ \x y -> 
   (-x)*y ~> -(x*y)
   
ruleDistrNegDiv = rule2 "Distr neg /" $ \x y -> 
   (-x)/y ~> -(x/y)
   
ruleDistrNegDenom = rule2 "Distr neg denom" $ \x y -> 
   x/(-y) ~> -(x/y)
   
-----------------------------------------------------------------------
-- Remaining distribution rules

ruleDistrPlusTimes                                       :: (MetaVar a, Num a)        => Rule a
ruleDistrTimesDiv, ruleDistrDivNumer, ruleDistrDivDenom  :: (MetaVar a, Fractional a) => Rule a
ruleDistrSqrtTimes, {-ruleDistrSqrtDiv,-} ruleDistrSqrtDenom :: (MetaVar a, Floating a)   => Rule a

ruleDistrPlusTimes = rule3 "Distr + *" $ \x y z -> 
   x*(y+z) ~> (x*y)+(x*z)
   
ruleDistrTimesDiv = rule3 "Distr * /" $ \x y z -> 
   x*(y/z) ~> (x*y)/z
   
ruleDistrDivNumer = rule3 "Distr / numer" $ \x y z -> 
   (x/y)/z ~> x/(y*z)
   
ruleDistrDivDenom = rule3 "Distr / denom" $ \x y z -> 
   x/(y/z) ~> x*(z/y)
   
ruleDistrSqrtTimes = rule2 "Distr sqrt *" $ \x y -> 
   sqrt x * sqrt y ~> sqrt (x*y)   .#. (x.>=0) /\ (y.>=0)
  
-- False! 
--ruleDistrSqrtDiv = rule2 "Distr sqrt /" $ \x y -> 
--   sqrt (x/y) ~> sqrt x / sqrt y
   
ruleDistrSqrtDenom = rule2 "Distr sqrt denom" $ \x y -> 
   x/sqrt y ~> (x*sqrt y)/y