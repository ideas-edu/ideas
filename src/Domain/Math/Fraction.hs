module Domain.Math.Fraction 
   (fractionExercise, cleanUpStrategy, go) where

import Prelude hiding (repeat)
import Control.Monad
import Test.QuickCheck hiding (label)
import Common.Apply
import Common.Context
import Common.Exercise
import Common.Strategy hiding (fail)
import Common.Transformation
import Common.Uniplate hiding (somewhere)
import Domain.Math.Expr
import Domain.Math.Parser
import Domain.Math.Views

------------------------------------------------------------
-- Exercise

fractionExercise :: Exercise Expr
fractionExercise = makeExercise 
   { identifier    = "fraction"
   , domain        = "math"
   , description   = "simplify fraction"
   , status        = Experimental
   , parser        = parseExpr
   , equality      = \a b -> cleanUpExpr a == cleanUpExpr b
   , equivalence   = (~=)
   , finalProperty = \a -> isFraction a || isInteger a || a == bottom
   , ruleset       = fractionRules
   , strategy      = fractionStrategy
   , generator     = genFraction 30
   }

------------------------------------------------------------
-- Strategy

fractionStrategy :: LabeledStrategy (Context Expr)
fractionStrategy = cleanUpStrategy (fmap cleanUpExpr) $ label "fraction" $ 
   repeat $ somewhere $ alternatives fractionRules

------------------------------------------------------------
-- Clean up: only some terms that look really "odd"

cleanUpStrategy :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategy f s = mapRules g (label (strategyName s) (doAfter f idRule <*> unlabel s))
 where
   g r | isMajorRule r = doAfter f r  
       | otherwise     = r
 
cleanUpExpr :: Expr -> Expr
cleanUpExpr = checkBottom . rewrite f
 where
   f :: Expr -> Maybe Expr
   f (Negate (Nat 0))    = return 0
   f (Negate (Negate a)) = return a
   f (a :+: Negate b)    = return (a - b)
   f (a :-: Negate b)    = return (a + b)
   f (_ :/: Nat 0)       = return bottom
   f _                   = Nothing
   
   checkBottom :: Expr -> Expr
   checkBottom e 
      | any (==bottom) (universe e) = bottom
      | otherwise                   = e

------------------------------------------------------------
-- Rules

fractionRules :: [Rule (Context Expr)]
fractionRules = map liftRuleToContext 
   [ plusCon, timesCon, minCon, divCon
   , plusZero, timesZero, timesOne
   , negateFraction, conTimesFraction, fractionTimesFraction
   , fractionNumerator, fractionDenominator
   , sameDenominator, plusFraction, conPlusFraction
   , simplerFraction
   ]

-- combining constants
plusCon :: Rule Expr
plusCon = makeSimpleRule "plusCon" f
 where
   f (e1 :+: e2) = do 
      a <- match integerView e1 
      b <- match integerView e2
      return (fromInteger (a+b))
   f _ = Nothing

timesCon :: Rule Expr
timesCon = makeSimpleRule "timesCon" f 
 where
   f (e1 :*: e2) = do
      a <- match integerView e1
      b <- match integerView e2 
      return (fromInteger (a*b))
   f _ = Nothing
   
minCon :: Rule Expr
minCon = makeSimpleRule "minCon" f
 where
   f (e1 :-: e2) = do
      a <- match integerView e1
      b <- match integerView e2
      return (fromInteger (a-b))
   f _ = Nothing 

divCon :: Rule Expr
divCon = makeSimpleRule "divCon" f
 where
   f e = do
      (a, b) <- match fractionView e
      let (n, zero) = a `divMod` b
      guard (b /= 0 && zero == 0)
      return (fromInteger n)

timesZero :: Rule Expr
timesZero = makeSimpleRule "timesZero" f
 where
   f (0 :*: x) | wf x = return 0
   f (x :*: 0) | wf x = return 0
   f _ = Nothing

timesOne :: Rule Expr
timesOne = makeSimpleRule "timesOne" f
 where
   f (1 :*: x) = return x
   f (x :*: 1) = return x
   f _ = Nothing

plusZero :: Rule Expr
plusZero = makeSimpleRule "plusZero" f
 where
   f (0 :+: x) = return x
   f (x :+: 0) = return x
   f _ = Nothing

simplerFraction :: Rule Expr
simplerFraction = makeSimpleRule "simplerFraction" f
 where
   f e = do 
      (a, b) <- match fractionView e
      guard (a /= 0 && b /= 0)
      let n = gcd a b
      guard (n `notElem` [0,1])
      return (fromInteger (a `div` n) / fromInteger (b `div` n))   

negateFraction :: Rule Expr
negateFraction = makeSimpleRule "negateFraction" f
 where
   f (Negate a :/: Negate b) = return (a/b)
   f (Negate a :/: b) = return (Negate (a/b))
   f (a :/: Negate b) = return (Negate (a/b))
   f _ = Nothing

conTimesFraction :: Rule Expr
conTimesFraction = makeSimpleRule "conTimesFraction" f
 where
   f (e1 :*: e2) = do
      (a, b) <- match fractionView e2
      n <- match integerView e1
      return (fromInteger (a*n) :/: fromInteger b)
    `mplus` do
      (a, b) <- match fractionView e1
      n <- match integerView e2
      return (fromInteger (a*n) :/: fromInteger b)
   f _ = Nothing
   
fractionTimesFraction :: Rule Expr
fractionTimesFraction = makeSimpleRule "fractionTimesFraction" f
 where
   f (e1 :*: e2) = do
      (a, b) <- match fractionView e1
      (c, d) <- match fractionView e2
      return (fromInteger (a*c)/fromInteger (b*d))
   f _ = Nothing

fractionNumerator :: Rule Expr
fractionNumerator = makeSimpleRule "fractionNumerator" f
 where
   f ((a :/: b) :/: c) = return (a/(b*c))
   f _ = Nothing

fractionDenominator :: Rule Expr
fractionDenominator = makeSimpleRule "fractionDenominator" f
 where
   f (a :/: (b :/: c)) | wf (b/c) = return (a*(c/b))
   f _ = Nothing
   
sameDenominator :: Rule Expr
sameDenominator = makeSimpleRule "sameDenominator" f
 where
   f e = do
      (e1, e2) <- match plusView e
      (a, b)   <- match fractionView e1
      (c, d)   <- match fractionView e2
      guard (b/=d && b/=0 && d/=0)
      let n = lcm b d
      if (n/=b) then 
         return ((fromInteger (a*(n `div` b)) :/: fromInteger n) :+: e2)
         else if (n/=d) then
         return (e1 :+: (fromInteger (c*(n `div` d)) :/: fromInteger n))
         else Nothing

plusFraction :: Rule Expr
plusFraction = makeSimpleRule "plusFraction" f
 where
   f e = do
      (e1, e2) <- match plusView e
      (a, b)   <- match fractionView e1
      (c, d)   <- match fractionView e2
      guard (b==d)
      return (fromInteger (a+c)/fromInteger d)

conPlusFraction :: Rule Expr
conPlusFraction = makeSimpleRule "conPlusFraction" f
 where
   f e = do
      (e1, e2) <- match plusView e
      a        <- match integerView e1
      (b, c)   <- match fractionView e2
      return (fromInteger (b+a*c) :/: fromInteger c)
    `mplus` do
      (e1, e2) <- match plusView e
      (b, c)   <- match fractionView e1
      a        <- match integerView e2
      return (fromInteger (b+a*c) :/: fromInteger c)
   
wf :: Expr -> Bool -- pessimistic approach
wf (a :/: b) = case exprToFractional b of
                  Just r  -> r/=0 && wf a 
                  Nothing -> False
wf e = e /= bottom && all wf (children e)
   
------------------------------------------------------------
-- Testing and checking

go  = replicateM 10 go1 >> replicateM 10 go2
go1 = quickCheck $ forAll (sized genFraction) prop1
go2 = quickCheck $ forAll (sized genFraction) prop2

prop1 :: Expr -> Bool
prop1 e = isFraction a || isInteger a || a == bottom
 where a = solveFraction e

prop2 :: Expr -> Bool 
prop2 e = (e ~= solveFraction e)

solveFraction :: Expr -> Expr
solveFraction = fromContext . applyD fractionStrategy . inContext

infix 1 ~=

(~=) :: Expr -> Expr -> Bool
e1 ~= e2 = exprToFractional e1 == (exprToFractional e2 :: Maybe Rational)
   
{- (~=) :: Expr -> Expr -> Property
e1 ~= e2 = forAll arbitrary $ \f -> 
   let g :: Expr -> Maybe Rational 
       g = exprToFractional . rec
       rec (Var s) = f s
       rec e = h (map rec cs)
        where (cs, h) = uniplate e
   in g e1 == g e2 -}

isInteger :: Expr -> Bool
isInteger e = e `belongsTo` integerView

isFraction :: Expr -> Bool
isFraction e = e `belongsTo` fractionView

genFraction :: Int -> Gen Expr
genFraction n 
   | n==0 = liftM (fromInteger . abs) arbitrary
   | otherwise = oneof 
        [ liftM2 (+) (genFraction h) (genFraction h)
        , liftM2 (-) (genFraction h) (genFraction h)
        , liftM (negate) (genFraction h)
        , liftM2 (*) (genFraction h) (genFraction h)
        , liftM2 (/) (genFraction h) (genFraction h)
        , genFraction 0
        ]
 where h = n `div` 2
 
-- sub-strategy for adding two fractions 
{- addTwoFractions :: Strategy (Context Expr)
addTwoFractions =  ruleLCM 
               <*> moveDown 0 <*> try scaleToLCM <*> moveUp
               <*> moveDown 1 <*> try scaleToLCM <*> moveUp
               <*> addParts
 where
   lcmVar = integerVar "lcm"
 
   ruleLCM = minorRule $ makeSimpleRule "ruleLCM" $ \c -> do
      e <- currentFocus c
      case e of
         (_ :/: e1) :+: (_ :/: e2) -> do
            a <- intV e1
            b <- intV e2
            return (set lcmVar (lcm a b) c)
         _ -> Nothing
   
   scaleToLCM = makeSimpleRule "scaleToLCM" $ \c -> do
      e <- currentFocus c
      case e of
         e1 :/: e2 -> do
            b <- intV e2
            let n   = get lcmVar c
                new = (e1 * Con (n `div` b)) / Con n -- but immediately simplify e1*...
            guard (n `mod` b == 0 && n /= b)
            return (changeFocus (const new) c)
         _ -> Nothing
   
   addParts = makeSimpleRule "addParts" $ \c -> do
      e <- currentFocus c
      case e of
         (a :/: n) :+: (b :/: m) | n==m -> do
            let new = (a+b) / n -- but immediately simplify a+b
            return (changeFocus (const new) c)
         _ -> Nothing
   
   moveDown i = minorRule $ makeSimpleRule ("moveDown["++show i++"]") $ \c -> return $ 
      changeLocation (locationDown i) c
   
   moveUp = minorRule $ makeSimpleRule "moveUp" $ \c -> do
      l <- locationUp (location c)
      return (setLocation l c)
      -}