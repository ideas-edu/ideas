module Domain.Math.Rules where

import Common.Rewriting
import Data.List

-----------------------------------------------------------------------
--  RewriteRule collections

numRules        :: (Rewrite a, Num a)        => [ RewriteRule a]
fractionalRules :: (Rewrite a, Fractional a) => [ RewriteRule a]
floatingRules   :: (Rewrite a, Floating a)   => [ RewriteRule a]
allRules        :: (Rewrite a, Floating a)   => [ RewriteRule a]

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

ruleCommPlus, ruleAssocPlus, ruleZeroPlus, ruleZeroPlusComm :: (Rewrite a, Num a) =>  RewriteRule a

ruleCommPlus = rewriteRule "Comm +" $ \x y -> 
   x+y :~> y+x 
   
ruleAssocPlus = rewriteRule "Assoc +" $ \x y z -> 
   (x+y)+z :~> x+(y+z)
   
ruleZeroPlus = rewriteRule "Zero +" $ \x -> 
   0+x :~> x
   
ruleZeroPlusComm = rewriteRule "Zero + Comm" $ \x -> 
   x+0 :~> x
   
-----------------------------------------------------------------------
-- Basic rules Times

ruleCommTimes, ruleAssocTimes, ruleZeroTimes, ruleZeroTimesComm, ruleOneTimes, ruleOneTimesComm :: (Rewrite a, Num a) =>  RewriteRule a

ruleCommTimes = rewriteRule "Comm *" $ \x y -> 
   x*y :~> y*x 
   
ruleAssocTimes = rewriteRule "Trans *" $ \x y z -> 
   (x*y)*z :~> x*(y*z)
   
ruleZeroTimes = rewriteRule "Zero *" $ \x -> 
   0*x :~> 0

ruleZeroTimesComm = rewriteRule "Zero * Comm" $ \x -> 
   x*0 :~> 0
   
ruleOneTimes = rewriteRule "One *" $ \x -> 
   1*x :~> x
   
ruleOneTimesComm = rewriteRule "One * Comm" $ \x -> 
   x*1 :~> x
   
-----------------------------------------------------------------------
-- Basic rules Negation

ruleInvNeg, ruleZeroNeg :: (Rewrite a, Num a) =>  RewriteRule a

ruleInvNeg = rewriteRule "Inv neg" $ \x ->
   -(-x) :~> x
   
ruleZeroNeg = rewriteRule "Zero neg" $ 
   -0 :~> 0
   
-----------------------------------------------------------------------
-- Basic rules Division

ruleZeroDiv, ruleOneDiv :: (Rewrite a, Fractional a) =>  RewriteRule a

ruleZeroDiv = rewriteRule "Zero /" $ \x ->
   0/x :~> 0   -- .#. x./=0
   
ruleOneDiv = rewriteRule "One /" $ \x -> 
   x/1 :~> x
   
-----------------------------------------------------------------------
-- Basic rules Square Roots

ruleZeroSqrt, ruleOneSqrt :: (Rewrite a, Floating a) =>  RewriteRule a

ruleZeroSqrt = rewriteRule "Zero sqrt" $ 
   sqrt 0 :~> 0
   
ruleOneSqrt = rewriteRule "One sqrt" $ 
   sqrt 1 :~> 1

-----------------------------------------------------------------------
-- Simplification rules

ruleSimplPlusNeg, ruleSimplPlusNegComm :: (Rewrite a, Num a)        =>  RewriteRule a
ruleSimplDiv, ruleSimplDivTimes        :: (Rewrite a, Fractional a) =>  RewriteRule a
ruleSimpleSqrtTimes                    :: (Rewrite a, Floating a)   =>  RewriteRule a

ruleSimplPlusNeg = rewriteRule "Simpl + neg" $ \x ->
   -x+x :~> 0

ruleSimplPlusNegComm = rewriteRule "Simpl + neg Comm" $ \x ->
   x+(-x) :~> 0
   
ruleSimplDiv = rewriteRule "Simpl /" $ \x -> 
   x/x :~> 1   -- .#. x./=0
   
ruleSimplDivTimes = rewriteRule "Simpl / *" $ \x y z -> 
   (x*y)/(x*z) :~> y/z   -- .#. x./=0

ruleSimpleSqrtTimes = rewriteRule "Simpl sqrt *" $ \x -> 
   sqrt x*sqrt x :~> x   -- .#. x.>=0

-----------------------------------------------------------------------
-- Distribution rules for Negation

ruleDistrNegPlus, ruleDistrNegTimes :: (Rewrite a, Num a)      =>  RewriteRule a
ruleDistrNegDiv, ruleDistrNegDenom  :: (Rewrite a, Floating a) =>  RewriteRule a

ruleDistrNegPlus = rewriteRule "Distr neg +" $ \x y -> 
   -(x+y) :~> (-x)+(-y)
   
ruleDistrNegTimes = rewriteRule "Distr neg *" $ \x y -> 
   (-x)*y :~> -(x*y)
   
ruleDistrNegDiv = rewriteRule "Distr neg /" $ \x y -> 
   (-x)/y :~> -(x/y)
   
ruleDistrNegDenom = rewriteRule "Distr neg denom" $ \x y -> 
   x/(-y) :~> -(x/y)
   
-----------------------------------------------------------------------
-- Remaining distribution rules

ruleDistrPlusTimes                                       :: (Rewrite a, Num a)        =>  RewriteRule a
ruleDistrTimesDiv, ruleDistrDivNumer, ruleDistrDivDenom  :: (Rewrite a, Fractional a) =>  RewriteRule a
ruleDistrSqrtTimes, {-ruleDistrSqrtDiv,-} ruleDistrSqrtDenom :: (Rewrite a, Floating a)   =>  RewriteRule a

ruleDistrPlusTimes = rewriteRule "Distr + *" $ \x y z -> 
   x*(y+z) :~> (x*y)+(x*z)
   
ruleDistrTimesDiv = rewriteRule "Distr * /" $ \x y z -> 
   x*(y/z) :~> (x*y)/z
   
ruleDistrDivNumer = rewriteRule "Distr / numer" $ \x y z -> 
   (x/y)/z :~> x/(y*z)
   
ruleDistrDivDenom = rewriteRule "Distr / denom" $ \x y z -> 
   x/(y/z) :~> x*(z/y)
   
ruleDistrSqrtTimes = rewriteRule "Distr sqrt *" $ \x y -> 
   sqrt x * sqrt y :~> sqrt (x*y)   -- .#. (x.>=0) /\ (y.>=0)
  
-- False! 
--ruleDistrSqrtDiv = rewriteRule "Distr sqrt /" $ \x y -> 
--   sqrt (x/y) :~> sqrt x / sqrt y
   
ruleDistrSqrtDenom = rewriteRule "Distr sqrt denom" $ \x y -> 
   x/sqrt y :~> (x*sqrt y)/y