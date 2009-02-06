module Domain.Math.Fraction (fractionExercise) where

import Prelude hiding (repeat)
import Control.Monad
import Test.QuickCheck hiding (label)
import Common.Apply
import Common.Context
import Common.Exercise
import Common.Strategy hiding (fail)
import Common.Transformation
import Common.Uniplate hiding (somewhere)
import Common.View
import Domain.Math.Expr
import Domain.Math.Parser
import Domain.Math.Symbolic

------------------------------------------------------------
-- Exercise

fractionExercise :: Exercise Expr
fractionExercise = makeExercise 
   { identifier    = "fraction"
   , domain        = "math"
   , description   = "simplify fraction"
   , status        = Experimental
   , parser        = parseExpr
   , equality      = \x y -> cleanUpExpr x == cleanUpExpr y
   , equivalence   = (~=)
   , finalProperty = \a -> isFraction a || isInteger a || a == symbol "_|_"
   , ruleset       = allRules
   , strategy      = fractionStrategy
   , generator     = genFraction 30
   }

------------------------------------------------------------
-- Strategy

solveFraction :: Expr -> Expr
solveFraction = fromContext . applyD fractionStrategy . inContext

fractionStrategy :: LabeledStrategy (Context Expr)
fractionStrategy = cleanUp (fmap cleanUpExpr) $ label "fraction" $ 
   repeat $ somewhere $ alternatives allRules

------------------------------------------------------------
-- Clean up: only some terms that look really "odd"

cleanUp :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUp f s = mapRules g (label (strategyName s) (doAfter f idRule <*> unlabel s))
 where
   g r | isMajorRule r = doAfter f r  
       | otherwise     = r
 
cleanUpExpr :: Expr -> Expr
cleanUpExpr = rewrite f
 where
   f :: Expr -> Maybe Expr
   f (Negate (Nat 0))    = return 0
   f (Negate (Negate a)) = return a
   f (a :+: Negate b)    = return (a - b)
   f (a :-: Negate b)    = return (a + b)
   f _                   = Nothing

------------------------------------------------------------
-- Rules

allRules :: [Rule (Context Expr)]
allRules = map liftRuleToContext 
   [ plusZero, timesZero, timesOne, plusCon, timesCon, minCon 
   , negateFraction, conTimesFraction, fractionTimesFraction
   , fractionNumerator, fractionDenominator
   , sameDenominator, plusFraction, conPlusFraction
   , divByZero, simplerFraction
   ]

plusCon :: Rule Expr
plusCon = makeSimpleRule "plusCon" f
 where
   f (e1 :+: e2) = do 
      a <- match integerView e1 
      b <- match integerView e2
      return (fromInteger (a+b))
   f _ = Nothing

timesZero :: Rule Expr
timesZero = makeSimpleRule "timesZero" f
 where
   f (0 :*: x) = return (if wf x then 0 else symbol "_|_")
   f (x :*: 0) = return (if wf x then 0 else symbol "_|_")
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

simplerFraction :: Rule Expr
simplerFraction = makeSimpleRule "simplerFraction" f
 where
   f e = do 
      (a, b) <- match fractionView e
      guard (a /= 0 && b /= 0)
      let n = gcd a b
      if b==n then return (fromInteger (a `div` n))
              else guard (n `notElem` [0,1])
                >> return (fromInteger (a `div` n) / fromInteger (b `div` n))   

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
   f (a :/: (b :/: c)) = return (if wf (b/c) then a*(c/b) else symbol "_|_")
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

divByZero :: Rule Expr
divByZero = makeSimpleRule "divByZero" f
 where
   f (_ :/: Nat 0) = return (symbol "_|_")
   f e = let xs = [ x | x <- universe e, x/=e, x==symbol "_|_" ]
         in if null xs then Nothing else return (symbol "_|_")
   
wf :: Expr -> Bool
wf (a :/: b) = wf a && wf b && exprToFractional b /= Just 0 
wf (Sym "_|_" []) = False
wf e = all wf (children e) 
   
------------------------------------------------------------
-- Views

plusView :: View Expr (Expr, Expr)
plusView = makeView f (uncurry (+))
 where
   f (a :+: b) = return (a, b)
   f (a :-: b) = return (a, negate b)
   f _         = Nothing

integerView :: View Expr Integer
integerView = makeView f fromInteger
 where
   f (Nat n)    = return n
   f (Negate e) = fmap negate (f e)
   f _          = Nothing

fractionView :: View Expr (Integer, Integer) -- second component is positive
fractionView = makeView f g
 where
   f (a :/: b)  = do 
      x <- match integerView a
      y <- match integerView b
      case compare y 0 of
         LT -> return (negate x, abs y)
         EQ -> fail "division by zero"
         GT -> return (x, y)
   f (Negate e) = fmap (\(x, y) -> (negate x, y)) (f e)
   f _          = Nothing
   
   g (x, y) = build integerView x / build integerView y

------------------------------------------------------------
-- Testing and checking

go  = replicateM 10 go1 >> replicateM 10 go2
go1 = quickCheck $ forAll (sized genFraction) prop1
go2 = quickCheck $ forAll (sized genFraction) prop2

prop1 :: Expr -> Bool
prop1 e = isFraction a || isInteger a || a == symbol "_|_"
 where a = solveFraction e

prop2 :: Expr -> Bool 
prop2 e = (e ~= solveFraction e)

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