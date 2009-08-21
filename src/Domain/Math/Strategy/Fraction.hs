module Domain.Math.Strategy.Fraction 
   ( fractionExercise, calculationExercise, go
   ) where

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
import Domain.Math.ExercisesDWO
import Domain.Math.Expr.Parser
import Domain.Math.Expr.Symbols
import Domain.Math.Expr.Symbolic
import Domain.Math.View.Basic
import Data.Maybe

bottom = symbol bottomSymbol

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
   , termGenerator = simpleGenerator (genFraction 30)
   }

calculationExercise :: Exercise Expr
calculationExercise = fractionExercise
   { identifier    = "calculation"
   , description   = "calculate result (basic)"
   , termGenerator = ExerciseList (concat calculateResults)
   }

------------------------------------------------------------
-- Strategy

fractionStrategy :: LabeledStrategy (Context Expr)
fractionStrategy = cleanUpStrategy (fmap cleanUpExpr) $ label "fraction" $ 
   repeat $ somewhere (alternatives fractionRules) |> somewhere addTwoFractions

------------------------------------------------------------
-- Clean up: only some terms that look really "odd"
 
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
fractionRules = map liftToContext 
   [ plusCon, timesCon, minCon, divCon
   -- , plusZero, timesZero, timesOne
   {- , negateFraction-} , conTimesFraction, fractionTimesFraction
   , fractionNumerator, fractionDenominator
   {- , sameDenominator, plusFraction conPlusFraction -}
   , simplerFraction
   ]

-- combining constants
plusCon :: Rule Expr
plusCon = makeSimpleRule "plusCon" f
 where
   f (e1 :+: e2) = do 
      a <- match conView e1 
      b <- match conView e2
      return (fromInteger (a+b))
   f _ = Nothing

timesCon :: Rule Expr
timesCon = makeSimpleRule "timesCon" f 
 where
   f (e1 :*: e2) = do
      a <- match conView e1
      b <- match conView e2 
      return (fromInteger (a*b))
   f _ = Nothing
   
minCon :: Rule Expr
minCon = makeSimpleRule "minCon" f
 where
   f (e1 :-: e2) = do
      a <- match conView e1
      b <- match conView e2
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
      guard (n `notElem` [0,1, b])
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
      n <- match conView e1
      return (fromInteger (a*n) :/: fromInteger b)
    `mplus` do
      (a, b) <- match fractionView e1
      n <- match conView e2
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
   f e = do 
      (e1, c) <- divV e
      (a, b)  <- divV e1
      return (a/(b*c))

fractionDenominator :: Rule Expr
fractionDenominator = makeSimpleRule "fractionDenominator" f
 where
   f e = do
      (a, e1) <- divV e
      (b, c)  <- divV e1
      guard (wf (b/c))
      return (a*(c/b))
   
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
      a        <- match conView e1
      (b, c)   <- match fractionView e2
      return (fromInteger (b+a*c) :/: fromInteger c)
    `mplus` do
      (e1, e2) <- match plusView e
      (b, c)   <- match fractionView e1
      a        <- match conView e2
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
prop1 e = isFraction a || isInteger a || isJust (isSymbol bottomSymbol a)
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
isInteger e = e `belongsTo` conView

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
addTwoFractions :: Strategy (Context Expr)
addTwoFractions =  ruleLCM 
               <*> repeat (moveDown <*> (scaleToLCM <|> scaleCon) <*> moveUp)
               <*> addParts
               <*> option (liftToContext simplerFraction <|> liftToContext divCon)
 where
   lcmVar = integerVar "lcm"
 
   ruleLCM = minorRule $ makeSimpleRule "ruleLCM" $ \c -> do
      e <- currentFocus c
      (e1, e2) <- match plusView e
      let f = fmap (either snd (const 1)) . match (eitherV fractionView conView)
                -- fmap snd (match fractionView e)-- `mplus` 
                --fmap (const 1) (match conView e)
      a <- f e1
      b <- f e2
      let v = lcm a b
      guard (v /= 1)
      return (set lcmVar v c)
   
   scaleToLCM = makeSimpleRule "scaleToLCM" $ \c -> do
      e <- currentFocus c
      (a, b) <- match fractionView e
      let n   = get lcmVar c
          new = (fromInteger (a*n `div` b)) / Nat n
      guard (n `mod` b == 0 && n /= b)
      return (changeFocus (const new) c)
   
   scaleCon = makeSimpleRule "scaleToLCM" $ \c -> do
      e <- currentFocus c
      a <- match conView e
      let n   = get lcmVar c
          new = (fromInteger (a*n)) / Nat n
      return (changeFocus (const new) c)
   
   addParts = makeSimpleRule "addParts" $ \c -> do
      e <- currentFocus c
      (e1, e2) <- match plusView e
      (a, n)   <- match fractionView e1
      (b, m)   <- match fractionView e2
      guard (n==m)
      let new = fromInteger (a+b) / fromInteger n
      return (changeFocus (const new) c)
   
   moveDown :: Rule (Context Expr)
   moveDown = minorRule $ makeSimpleRuleList "moveDown" $ \c ->
      case currentFocus c of 
         Just e  -> [ changeLocation (locationDown i) c | i <- [0 .. length (children e)-1] ]
         Nothing -> []
   
   moveUp = minorRule $ makeSimpleRule "moveUp" $ \c -> do
      l <- locationUp (location c)
      return (setLocation l c)
      
   intV :: Expr -> Maybe Integer
   intV = match conView
   
divV :: Expr -> Maybe (Expr, Expr)
divV (a :/: b)  = Just (a, b)
divV (Negate a) = fmap (\(x, y) -> (negate x, y)) (divV a)
divV _ = Nothing

--second :: View a b -> View (c, a) (c, b)
--second v = makeView (\(c, a) -> match v a >>= \b -> return (c, b)) (\(c, b) -> (c, build v b))

newV :: View Expr (Expr, Integer)
newV = makeView divV undefined >>> second conView

-- left-biased
eitherV :: View a b -> View a c -> View a (Either b c)
eitherV v1 v2 = makeView (\a -> fmap Left (match v1 a) `mplus` fmap Right (match v2 a)) (either (build v1) (build v2))