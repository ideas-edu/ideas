{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- some of these help functions may have a broader scope and could be 
-- moved to other parts of the framework (eg. Common)
--
-----------------------------------------------------------------------------

module Domain.Math.Power.Utils where

import Prelude hiding (repeat, replicate)

import Common.Context
import Common.Rewriting
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Uniplate
import Common.View
import Control.Monad
import Data.Foldable (Foldable, foldMap, toList)
import Data.Function (on)
import Data.List hiding (repeat, replicate)
import Data.Ratio
import Data.Traversable (Traversable, mapM)
import qualified Domain.Math.Data.PrimeFactors as PF
import Domain.Math.CleanUp
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Expr
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Views


-- | Strategy functions -------------------------------------------------------

exhaustiveStrategy :: IsTerm a => [Rule a] -> Strategy (Context a)
exhaustiveStrategy = exhaustiveSomewhere . map liftToContext

exhaustiveUse :: (IsTerm a, IsTerm b) => [Rule a] -> Strategy (Context b)
exhaustiveUse = exhaustiveSomewhere . map use

exhaustiveSomewhere :: IsStrategy f => [f (Context a)] -> Strategy (Context a)
exhaustiveSomewhere = repeat . somewhere . alternatives

-- | Rule functions -----------------------------------------------------------

smartRule :: Rule Expr -> Rule Expr
smartRule = doAfter f
  where
    f (a :*: b) = a .*. b
    f (a :/: b) = a ./. b
    f (Negate a) = neg a
    f (a :+: b) = a .+. b
    f (a :-: b) = a .-. b
    f e = e
         
mergeConstantsWith :: (Expr -> Bool) -> Expr -> Expr
mergeConstantsWith p = simplifyWith f productView
  where
    f (sign, xs) = 
      let (cs, ys) = partition p xs
          c = simplify rationalView $ build productView (False, cs)
      in if maybe False (> 1) (match rationalView c) 
           then (sign, c:ys) 
           else (sign, xs)

mergeConstants :: Expr -> Expr
mergeConstants = mergeConstantsWith (`belongsTo` rationalView)

-- | View functions -----------------------------------------------------------

(<&>) :: (MonadPlus m) => ViewM m a b -> ViewM m a b -> ViewM m a b
v <&> w = makeView (\x -> match v x `mplus` match w x) (build v)

infixl 1 <&>

plainNatView :: View Expr Integer
plainNatView = makeView f Nat
  where
    f (Nat n) = Just n
    f _       = Nothing

myIntegerView :: View Expr Integer
myIntegerView = makeView f fromInteger
  where
    f (Nat n)          = Just n
    f (Negate (Nat n)) = Just $ negate n
    f _                = Nothing

plainRationalView :: View Rational (Integer, Integer)
plainRationalView = 
  makeView (\x -> return (numerator x, denominator x)) (uncurry (%))

eqView :: View a b -> View (Equation a) (b, b)
eqView v = eqv >>> v *** v
  where
    eqv = makeView (\(lhs :==: rhs) -> Just (lhs, rhs)) (uncurry (:==:))

relationView :: View (Equation a) (Relation a)
relationView = makeView f g
 where
   f (x :==: y) = return $ x .==. y
   g r | relationType r == EqualTo = leftHandSide r :==: rightHandSide r
       | otherwise                 = error "Not an equality"

-- | Rule collections ---------------------------------------------------------

naturalRules :: [Rule Expr]
naturalRules =
   [ calcPlusWith "nat" plainNatView, calcMinusWith "nat" plainNatView
   , calcTimesWith "nat" plainNatView, calcDivisionWith "nat" plainNatView
   , doubleNegate, negateZero , plusNegateLeft, plusNegateRight
--   , minusNegateLeft
   , minusNegateRight, timesNegateLeft, timesNegateRight, divisionNegateLeft
   , divisionNegateRight
   ]

rationalRules :: [Rule Expr]
rationalRules = 
   [ calcPlusWith "rational" rationalRelaxedForm
   , calcMinusWith "rational" rationalRelaxedForm
   , calcTimesWith "rational" rationalRelaxedForm
   , calcDivisionWith "integer" integerNormalForm
   , doubleNegate, negateZero, divisionDenominator, divisionNumerator
   , simplerFraction
   ]
   
fractionRules :: [Rule Expr]
fractionRules =
   [ fractionPlus, fractionPlusScale, fractionTimes
   , calcPlusWith "integer" integerNormalForm
   , calcMinusWith "integer" integerNormalForm
   , calcTimesWith "integer" integerNormalForm -- not needed?
   , calcDivisionWith "integer" integerNormalForm
   , doubleNegate, negateZero, smartRule divisionDenominator
   , smartRule divisionNumerator, simplerFraction
   ]

coverUpRulesX :: [Rule (Equation Expr)]
coverUpRulesX = map (\r -> r cfg)
   [ coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith, coverUpNegateWith
   , coverUpTimesWith, coverUpNumeratorWith, coverUpDenominatorWith, coverUpSqrtWith
   ]
   where 
     cfg = configCoverUp { predicateCovered = elem "x" . vars
                         , predicateCombined = notElem "x" . vars 
                         , coverLHS = False} 

-- | Common functions ---------------------------------------------------------

sortExpr :: Expr -> Expr
sortExpr = transform $ simplifyWith (sort . map sortProd) sumView
  where sortProd = simplifyWith (fmap sort) productView

sortEquation :: Equation Expr -> Equation Expr
sortEquation (x :==: y) = if x < y then eq else flipSides eq
  where eq = sortExpr x :==: sortExpr y

sortOrList :: OrList (Equation Expr) -> OrList (Equation Expr)
sortOrList = toOrList . sort . map sortEquation . toList

-- Semantic equivalence
class SemEq a where
    (===), (=/=) :: a -> a -> Bool
    x =/= y = not (x === y)
--    x === y = not (x =/= y)

infix 4 ===, =/=

instance SemEq a => SemEq (Equation a) where
  (a :==: b) === (c :==: d) = a === c && b === d || a === d && b === c

instance SemEq Expr where
  (===) = on (==) cleanUpExpr

instance SemEq a => SemEq (OrList a) where
  a === b = let as = toList a ; bs = toList b
            in length (intersectBy (===) as bs) == length as

tryRewriteAll :: (a -> [a]) -> a -> [a]
tryRewriteAll f x = 
  case f x of
    [] -> [x]
    xs -> xs

transformList :: Uniplate a => (a -> [a]) -> a -> [a]
transformList f a = concatMap (f . ctx) $ g $ map (transformList f) cs
  where 
    (cs, ctx) = uniplate a
    g (xs:xss) = concatMap (\x -> map (x:) (g xss)) xs
    g []       = [[]]

transformOrList :: (Traversable f, Uniplate a) => (a -> [a]) -> OrList (f a) -> OrList (f a)
transformOrList f = foldMap (toOrList . Data.Traversable.mapM (transformList f))

-- y = root n x
takeRoot :: Integer -> Integer -> [Integer]
takeRoot n x | n == 0    = [0]
             | n == 1    = if x > 0 && odd x then [1] else [1, -1]
             | n == (-1) = if x > 0 && odd x then [-1] else []
             | x == 1    = [n]
             | x > 0     = maybe [] roots $ lookup x $ map swap $ PF.allPowers (abs n) 
             | otherwise = []
  where
    roots r | n > 0 && even x = [r, negate r]
            | n > 0 && odd  x = [r]
            | n < 0 && odd  x = [negate r]
            | otherwise       = []

-- prop_takeRoot n = traceShow n f
--   where
--     f n x | x > 0 = n `elem` (takeRoot (n Prelude.^ x) x)
--           | otherwise = True

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

split :: (Eq a) => (a -> a -> t) -> [a] -> [(t, [a])]    
split op xs = f xs
      where
        f (y:ys) | not (null ys) = [(y `op` z, xs \\ [y, z]) | z <- ys] ++ f ys 
                 | otherwise     = []
        f [] = []

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe p x = if p x then Just x else Nothing

joinBy :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
joinBy _  [] = []
joinBy eq xs = ys : joinBy eq (xs \\ ys)
  where
    ys = dropUntil eq xs 

dropUntil :: (a -> a -> Bool) -> [a] -> [a]
dropUntil _ []       = []
dropUntil _ [x]      = [x]
dropUntil p (x:y:ys) | p x y     = x : dropUntil p (y:ys) 
                     | otherwise = [x]

holes :: [a] -> [(a, [a], a -> [a])]
holes xs = map f [0 .. length xs - 1] 
  where 
    f i = let (ys, z:zs) = splitAt i xs 
          in (z, ys ++ zs, \x -> ys ++ x:zs)

twoNonAdjacentHoles :: [a] -> [((a, a), a -> [a])]
twoNonAdjacentHoles xs = concatMap g pairs
  where
    pairs = [(x, y) | x <- [0 .. length xs - 1], y <- [x + 1 .. length xs - 1]]
    g (x, y) = let (ys, z:zs) = splitAt x xs 
                   (ps, q:qs) = splitAt (y - x - 1) zs 
               in if null ps
                 then [ ((z, q), \a -> ys ++ a:ps ++ qs) ]
                 else [ ((z, q), \a -> ys ++ a:ps ++ qs)
                      , ((z, q), \a -> ys ++ ps ++ a:qs) ]
