module Domain.Math.QuadraticEquations where

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Uniplate (somewhereM, transform)
import Control.Monad
import Data.List (intersperse, (\\), sort, nub)
import Data.Ratio
import Domain.Math.Equation
import Domain.Math.ExercisesDWO
import Domain.Math.Expr
import Domain.Math.LinearEquations (solvedEquation, merge, distributionT, minusT, timesT, divisionT, solveEquation)
import Domain.Math.OrList
import Domain.Math.Parser
import Domain.Math.Polynomial 
import Domain.Math.Views
import Prelude hiding (repeat, (^))
import Test.QuickCheck hiding (label)
import qualified Domain.Math.SquareRoot as SQ

------------------------------------------------------------
-- Exercise

quadraticEquationExercise :: Exercise (OrList (Equation Expr))
quadraticEquationExercise = makeExercise 
   { identifier    = "quadreq"
   , domain        = "math"
   , description   = "solve a quadratic equation"
   , status        = Experimental
   , parser        = parseWith (pOrList (pEquation pExpr))
   , equality      = (==) 
   , equivalence   = viewEquivalent qView
   , finalProperty = solvedList
   , ruleset       = map ignoreContext allRules
   , strategy      = ignoreContext solverQ
   , generator     = oneof (map (return . OrList . return) $ concat quadraticEquations)
   }

------------------------------------------------------------
-- Strategy and lifting

solvedList :: OrList (Equation Expr) -> Bool
solvedList (OrList xs) = all solvedEquation xs

solve :: Equation Expr -> OrList (Equation Expr)
solve e = applyD solverQ (OrList [e])

solverQ :: LabeledStrategy (OrList (Equation Expr))
solverQ = cleanUpStrategy cleanUpOrs $
   label "Quadratic equations" $
      repeat ((coverUpPlus <|> coverUpTimes <|> coverUpNegate <|> coverUpSquare
         <|> coverUpDiv <|> cancelTerms <|> factorPower <|> mulZero <|> flipEquation
         ) |> (moveToLeft <|> niceFactors <|> distribution <|> distributionSquare <|> mergeR <|> simplerA) |> abcFormula) 

cleanUpOrs :: OrList (Equation Expr) -> OrList (Equation Expr)
cleanUpOrs (OrList xs) = OrList (map (fmap (f2 . f1 . simplifyExpr)) xs)
 where
   f1 = transform (simplify powerView)
   f2 = simplify squareRootView

linS :: Rule (OrList (Equation Expr))
linS = makeSimpleRuleList "linear equation" $ forOne $ \eq -> do
   match equationView eq
   cnew <- apply solveEquation (inContext eq)
   let new = fromContext cnew
   guard (new /= eq)
   return [new]
   
forOne :: (a -> Maybe [a]) -> OrList a -> [OrList a]
forOne f (OrList xs) = map OrList (rec xs)
 where
   rec []     = []
   rec (x:xs) = maybe [] (\y -> [y++xs]) (f x) ++ map (x:) (rec xs)

------------------------------------------------------------
-- Rule collection

allRules :: [Rule (OrList (Equation Expr))]
allRules = 
   [ coverUpPlus, coverUpTimes, coverUpNegate, coverUpSquare
   , coverUpDiv, cancelTerms, factorPower, mulZero, flipEquation
   , moveToLeft, niceFactors, distribution, distributionSquare 
   , mergeR, simplerA, abcFormula
   ]

------------------------------------------------------------
-- Rules

makeSqrt :: Expr -> Expr
makeSqrt (Nat n) | a*a == n = Nat a
 where a = SQ.isqrt n
makeSqrt e = sqrt e

-- X^2 = A  implies  X= +/- sqrt(A)
coverUpSquare :: Rule (OrList (Equation Expr))
coverUpSquare = makeSimpleRuleList "cover-up square" (forOne f)
 where
   f (Sym "^" [a, Nat 2] :==: rhs) | hasVars a && noVars rhs = do
      let e = makeSqrt rhs
      return [a :==: e, a :==: negate e]
   f _ = Nothing

coverUpPlus :: Rule (OrList (Equation Expr))
coverUpPlus = makeSimpleRuleList "cover-up plus" (forOne (fmap return . apply f))
 where
   f = flip supply1 minusT $ \(lhs :==: rhs) -> do
      guard (noVars rhs)
      (a, b) <- match plusView lhs
      let oneVar = (==1) . length . collectVars
      r <- case (match rationalView a, match rationalView b) of
              (Just r, _) | oneVar b -> Just r
              (_, Just r) | oneVar a -> Just r
              _                      -> Nothing
      guard (r /= 0)
      return (build rationalView r)

coverUpTimes :: Rule (OrList (Equation Expr))
coverUpTimes = makeSimpleRuleList "cover-up times" (forOne (fmap return . apply f))
 where
   f = flip supply1 divisionT $ \(lhs :==: rhs) -> do
      guard (noVars rhs)
      (a, b) <- match timesView lhs
      r <- case (match rationalView a, match rationalView b) of
              (Just r, _) | hasVars b -> Just r
              (_, Just r) | hasVars a -> Just r
              _                       -> Nothing
      guard (r `notElem` [0, 1])
      return (build rationalView r)

coverUpNegate :: Rule (OrList (Equation Expr))
coverUpNegate = makeSimpleRuleList "cover-up negate" (forOne f)
 where
   f (Negate a :==: b) | hasVars a && noVars b = 
      return [a :==: Negate b]
   f (a :==: Negate b) | noVars a && hasVars b =
      return [Negate a :==: b]
   f _ = Nothing

coverUpDiv :: Rule (OrList (Equation Expr))
coverUpDiv = makeSimpleRuleList "cover-up division" (forOne (fmap return . apply f))
 where
   f = flip supply1 timesT $ \(lhs :==: rhs) -> do
      guard (noVars rhs)
      (a, b) <- match divView lhs
      guard (hasVars a)
      r <- match rationalView b
      guard (r `notElem` [0, 1])
      return (build rationalView r)

cancelTerms :: Rule (OrList (Equation Expr))
cancelTerms = makeSimpleRuleList "cancel terms" $ forOne $ \(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   let without a as = build sumView (as \\ [a])
   case [ x | x <- xs, y <- ys, x==y ] of
      []   -> Nothing
      hd:_ -> return [without hd xs :==: without hd ys]

-- x^n == x^m   or    x^n+x^m ==0    (can this be combined?)
factorPower :: Rule (OrList (Equation Expr))
factorPower = makeSimpleRuleList "factor power" $ forOne $ \(lhs :==: rhs) -> do
   (e1, x1, n1) <- match powerView lhs
   (e2, x2, n2) <- match powerView rhs
   guard (x1==x2 && n1 > 0 && n2 > 0)
   let m = n1 `min` n2
       make e n = build powerView (e, x1, n-m)
   return [ Var x1 :==: 0, make e1 n1 :==: make e2 n2 ]
 `mplus` do
   guard (rhs == 0)
   (a, b) <- match plusView lhs
   (e1, x1, n1) <- match powerView a
   (e2, x2, n2) <- match powerView b
   guard (x1==x2 && n1 > 0 && n2 > 0)
   let m = n1 `min` n2
       make e n = build powerView (e, x1, n-m)
   return [ Var x1 :==: 0, make e1 n1 .+. make e2 n2 :==: 0 ]

mulZero :: Rule (OrList (Equation Expr))
mulZero = makeSimpleRuleList "multiplication is zero" $ forOne $ \(lhs :==: rhs) -> do
   (_, xs) <- match productView lhs
   guard (rhs == 0 && length xs > 1)
   return [ x :==: 0 | x <- xs ]

-- really needed?
flipEquation :: Rule (OrList (Equation Expr))
flipEquation = makeSimpleRuleList "flip equation" $ forOne $ \(lhs :==: rhs) -> do
   guard (noVars lhs && hasVars rhs)
   return [ rhs :==: lhs ]

moveToLeft :: Rule (OrList (Equation Expr))
moveToLeft = makeSimpleRuleList "move to left" (forOne (fmap return . apply f))
 where
   f = flip supply1 minusT $ \(lhs :==: rhs) -> do
      guard (rhs /= 0 && lhs /= Var "x")
      return rhs

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Rule (OrList (Equation Expr))
niceFactors = makeSimpleRuleList "nice factors" $ forOne $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   let sign t@(x, a, b, c) = if a== -1 then (x, 1, -b, -c) else t 
   (x, a, rb, rc) <- liftM sign (match quadraticView lhs)
   b <- isInt rb
   c <- isInt rc
   guard (a==1)
   case [ (Var x + Nat i) * (Var x + Nat j) | (i, j) <- factors c, i+j == b ] of
      hd:_ -> return [hd :==: 0]
      _    -> Nothing

factors :: Integer -> [(Integer, Integer)]
factors n = concat [ [(a, b), (negate a, negate b)] | a <- [1..h], let b = n `div` a, a*b == n ]
 where h = floor (sqrt (abs (fromIntegral n)))

distribution :: Rule (OrList (Equation Expr))
distribution = makeSimpleRuleList "distribution" (forOne f)
 where
   g = somewhereM (apply distributionT)
   f (lhs :==: rhs) = 
      case (g lhs, g rhs) of
         (Just new, _) -> return [new :==: rhs]
         (_, Just new) -> return [lhs :==: new]
         _             -> Nothing 

distributionSquare :: Rule (OrList (Equation Expr))
distributionSquare = makeSimpleRuleList "distribution square" (forOne f)
 where
   g (Sym "^" [x, Nat 2]) = do
      (a, x, b) <- match linearView x
      guard (a /= 0 && b /= 0)
      return  (  (fromRational (a*a) .*. (Var x^2)) 
             .+. (fromRational (2*a*b) .*. Var x)
             .+. (fromRational (b*b)))
   g _ = Nothing
   f (lhs :==: rhs) = 
      case (somewhereM g lhs, somewhereM g rhs) of
         (Just new, _) -> return [new :==: rhs]
         (_, Just new) -> return [lhs :==: new]
         _             -> Nothing 

mergeR :: Rule (OrList (Equation Expr))
mergeR = makeSimpleRuleList "merge" (forOne (fmap return . apply merge))

simplerA :: Rule (OrList (Equation Expr))
simplerA = makeSimpleRuleList "simpler A" $ (forOne f)
 where
   f (lhs :==: rhs) = do
      guard (rhs == 0)
      (x, ra, rb, rc) <- match quadraticView lhs
      [a, b, c] <- mapM isInt [ra, rb, rc] 
      let d = a `gcd` b `gcd` c
      guard (d `notElem` [0, 1])
      return [build quadraticView (x, fromInteger (a `div` d), fromInteger (b `div` d), fromInteger (c `div` d)) :==: 0]

abcFormula :: Rule (OrList (Equation Expr))
abcFormula = makeSimpleRuleList "abc formula" $ forOne $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, a, b, c) <- match quadraticView lhs
   let discr = makeSqrt (fromRational (b*b - 4 * a * c))
   case discr of Nat n -> guard (even n); _ -> return () -- no nice numbers (for now)
   return [ Var x :==: (-fromRational b + discr) / 2 * fromRational a
          , Var x :==: (-fromRational b - discr) / 2 * fromRational a
          ]

isInt :: Rational -> Maybe Integer
isInt r = do
   guard (denominator r == 1)
   return (numerator r)

------------------------------------------------------------
-- Testing

testAll = drop 0 $ zipWith f [1..] (concat quadraticEquations)
 where
   f i e
      | not (solvedList (solve e)) = 
           error (show e ++ "  becomes   " ++ show (solve e))
      | testD e   = 0
      | otherwise = i

testD :: Equation Expr -> Bool
testD e = 
   case derivations (unlabel solverQ) (OrList [e]) of
      [] -> error "no derivation"
      (a, ps):_ -> 
         let xs = a : map snd ps
         in case [ (x, y) | x <- xs, y <- xs, not (equivalence quadraticEquationExercise x y) ] of
               []   -> False
               (x,y):_ -> error $ show (x, y) -- (simplify qView x) ++ "    is not    " ++ show (simplify qView y)

main :: IO ()
main = showDerivations quadraticEquationExercise xs 
 where xs = map (OrList . return) (concat quadraticEquations)
 
qView :: View (OrList (Equation Expr)) [SQ.SquareRoot Rational]
qView = makeView f g
 where
   f (OrList xs) = liftM (sort . nub . filter (not . SQ.imaginary) . concat) $ mapM (match qView2) xs
   g xs = OrList [ Var "x" :==: build squareRootView rhs | rhs <- xs ] -- !!!!!! don't guess variable

qView2 :: View (Equation Expr) [SQ.SquareRoot Rational]
qView2 = makeView f undefined
 where
   f (lhs :==: rhs) = do
      (x, poly) <- match polyView (lhs - rhs)
      guard (degree poly <= 2)
      ra <- match rationalView (coefficient 2 poly)
      rb <- match rationalView (coefficient 1 poly)
      case ra==0 of
         True -> do
            rc <- match squareRootView (coefficient 0 poly)
            return [SQ.scale (-1/rb) rc]
         False -> do 
            rc <- match rationalView (coefficient 0 poly)
            let discr = rb*rb - 4*ra*rc
            case compare discr 0 of
               LT -> Just []
               EQ -> Just [SQ.con (-rb/(2*ra))]
               GT ->  
                  let sdiscr = SQ.sqrtRational discr
                  in return [ SQ.scale (1/(2*ra)) (-SQ.con rb + sdiscr)
                            , SQ.scale (1/(2*ra)) (-SQ.con rb - sdiscr)
                            ]

squareRootView :: View Expr (SQ.SquareRoot Rational)
squareRootView = makeView f g
 where
   f (Sqrt a)   = fmap SQ.sqrtRational (match rationalView a)
   f (Negate a) = fmap negate (f a)
   f (Nat a)    = Just (fromIntegral a)
   f (a :+: b)  = liftM2 (+) (f a) (f b)
   f (a :-: b)  = liftM2 (-) (f a) (f b)
   f (a :*: b)  = liftM2 (*) (f a) (f b)
   f (a :/: b)  = liftM2 (\s r -> s * SQ.con (1/r)) (f a) (match rationalView b)
   f e = Nothing
   
   g m = build sumView (map h (SQ.toList m))
   h (r, n)  
      | n == 0    = 0
      | n == 1    = fromRational r
      | otherwise = fromRational r .*. Sqrt (fromIntegral n)
   
polyView :: View Expr (String, Polynomial Expr)
polyView = makeView f undefined
 where
   f e = case nub (collectVars e) of
            [v] -> g e >>= \a -> Just (v, a)
            _   -> Nothing
   
   g (Var _)    = Just var
   g (Nat n)    = Just (fromIntegral n)
   g (Negate a) = liftM negate (g a)
   g (a :+: b)  = liftM2 (+) (g a) (g b)
   g (a :-: b)  = liftM2 (-) (g a) (g b)
   g (a :*: b)  = liftM2 (*) (g a) (g b)
   g (a :/: b)  = guard (noVars b) >> liftM (\x -> fmap (/b) x) (g a)
   g (Sym "^" [a, Nat n]) = liftM (`power` 2) (g a)
   g e@(Sqrt a) | noVars a = Just (con e)
   g e | noVars e  = Just (con e)
       | otherwise = Nothing