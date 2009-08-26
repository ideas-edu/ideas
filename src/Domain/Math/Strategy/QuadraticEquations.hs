module Domain.Math.Strategy.QuadraticEquations where

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Traversable
import Common.Uniplate (somewhereM, transform)
import Control.Monad
import Data.List ((\\), sort, nub)
import Data.Ratio
import Domain.Math.Data.Equation
import Domain.Math.ExercisesDWO
import Domain.Math.Expr
import Domain.Math.View.SquareRoot
import Domain.Math.View.Polynomial
import Domain.Math.Strategy.LinearEquations (solvedEquation, merge, distributionT, minusT, timesT, divisionT, solveEquation)
import Domain.Math.Data.OrList
import Domain.Math.Expr.Parser
import Domain.Math.Expr.Symbols
import Domain.Math.Data.Polynomial 
import Domain.Math.Simplification (smartConstructors)
import Domain.Math.View.Basic hiding (linearView)
import Prelude hiding (repeat, (^))
import qualified Prelude
import qualified Domain.Math.Data.SquareRoot as SQ

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
   , termGenerator = ExerciseList (map (OrList . return) $ concat quadraticEquations)
   }

------------------------------------------------------------
-- Strategy and liftings

solvedList :: OrList (Equation Expr) -> Bool
solvedList (OrList xs) = all solvedEquation xs

solve :: Equation Expr -> OrList (Equation Expr)
solve e = applyD solverQ (OrList [e])

solverQ :: LabeledStrategy (OrList (Equation Expr))
solverQ = cleanUpStrategy cleanUpOrs $
   label "Quadratic equations" $
      repeat ((coverUpPlus <|> coverUpTimes <|> coverUpNegate <|> coverUpSquare
         <|> coverUpDiv <|> cancelTerms <|> factorPower <|> mulZero <|> flipEquation
         ) |> (moveToLeft <|> niceFactors <|> distribution <|> distributionSquare <|> mergeR <|> simplerA) 
           |> abcFormula) 

cleanUpOrs :: OrList (Equation Expr) -> OrList (Equation Expr)
cleanUpOrs (OrList xs) = OrList (map (fmap (f2 . f1 . smartConstructors)) xs)
 where
   f1 = transform (simplify powerView)
   f2 = simplify squareRootView

linS :: Rule (OrList (Equation Expr))
linS = makeSimpleRule "linear equation" $ onceM  $ \eq -> do
   match equationView eq
   cnew <- apply solveEquation (inContext eq)
   let new = fromContext cnew
   guard (new /= eq)
   return new

{-
forOne :: (a -> Maybe [a]) -> OrList a -> [OrList a]
forOne f (OrList xs) = map OrList (rec xs)
 where
   rec []     = []
   rec (x:xs) = maybe [] (\y -> [y++xs]) (f x) ++ map (x:) (rec xs)

oneSide :: (a -> Maybe a) -> Equation a -> Maybe [Equation a]
oneSide f (lhs :==: rhs)
   | null xs   = Nothing
   | otherwise = Just xs
 where 
   xs = catMaybes [fmap (:==: rhs) (f lhs), fmap (lhs :==:) (f rhs)] -}

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
coverUpSquare = makeSimpleRuleList "cover-up square" (onceJoinM f)
 where
   f (Sym s [a, Nat 2] :==: rhs) | s == powerSymbol && hasVars a && noVars rhs = do
      let e = makeSqrt rhs
      return $ OrList [a :==: e, a :==: negate e]
   f _ = []

coverUpPlus :: Rule (OrList (Equation Expr))
coverUpPlus = makeSimpleRule "cover-up plus" (onceM  (apply f))
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
coverUpTimes = makeSimpleRule "cover-up times" (onceM  (apply f))
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
coverUpNegate = makeSimpleRuleList "cover-up negate" (onceM  f)
 where
   f (Negate a :==: b) | hasVars a && noVars b = 
      return (a :==: Negate b)
   f (a :==: Negate b) | noVars a && hasVars b =
      return (Negate a :==: b)
   f _ = []

coverUpDiv :: Rule (OrList (Equation Expr))
coverUpDiv = makeSimpleRule "cover-up division" (onceM  (apply f))
 where
   f = flip supply1 timesT $ \(lhs :==: rhs) -> do
      guard (noVars rhs)
      (a, b) <- match divView lhs
      guard (hasVars a)
      r <- match rationalView b
      guard (r `notElem` [0, 1])
      return (build rationalView r)

cancelTerms :: Rule (OrList (Equation Expr))
cancelTerms = makeSimpleRule "cancel terms" $ onceJoinM $ \(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   let without a as = build sumView (as \\ [a])
   case [ x | x <- xs, y <- ys, x==y ] of
      []   -> Nothing
      hd:_ -> return (OrList [without hd xs :==: without hd ys])

-- x^n == x^m   or    x^n+x^m ==0    (can this be combined?)
factorPower :: Rule (OrList (Equation Expr))
factorPower = makeSimpleRule "factor power" $ onceJoinM $ \(lhs :==: rhs) -> do
   (e1, x1, n1) <- match powerView lhs
   (e2, x2, n2) <- match powerView rhs
   guard (x1==x2 && n1 > 0 && n2 > 0)
   let m = n1 `min` n2
       make e n = build powerView (e, x1, n-m)
   return (OrList [ Var x1 :==: 0, make e1 n1 :==: make e2 n2 ])
 `mplus` do
   guard (rhs == 0)
   (a, b) <- match plusView lhs
   (e1, x1, n1) <- match powerView a
   (e2, x2, n2) <- match powerView b
   guard (x1==x2 && n1 > 0 && n2 > 0)
   let m = n1 `min` n2
       make e n = build powerView (e, x1, n-m)
   return (OrList [ Var x1 :==: 0, make e1 n1 .+. make e2 n2 :==: 0 ])

mulZero :: Rule (OrList (Equation Expr))
mulZero = makeSimpleRule "multiplication is zero" $ onceJoinM $ \(lhs :==: rhs) -> do
   (_, xs) <- match productView lhs
   guard (rhs == 0 && length xs > 1)
   return (OrList [ x :==: 0 | x <- xs ])

-- really needed?
flipEquation :: Rule (OrList (Equation Expr))
flipEquation = makeSimpleRule "flip equation" $ onceM  $ \(lhs :==: rhs) -> do
   --guard (noVars lhs && hasVars rhs) -- not sufficient
   (_, p1) <- match polyView lhs
   (_, p2) <- match polyView rhs
   guard (degree p1 < degree p2)
   return (rhs :==: lhs)
   
moveToLeft :: Rule (OrList (Equation Expr))
moveToLeft = makeSimpleRule "move to left" (onceM  (apply f))
 where
   f = flip supply1 minusT $ \(lhs :==: rhs) -> do
      guard (rhs /= 0 && lhs /= Var "x")
      return rhs

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Rule (OrList (Equation Expr))
niceFactors = makeSimpleRule "nice factors" $ onceM  $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   let sign t@(x, a, b, c) = if a== -1 then (x, 1, -b, -c) else t 
   (x, a, rb, rc) <- liftM sign (match (quadraticViewWith rationalView) lhs)
   b <- isInt rb
   c <- isInt rc
   guard (a==1)
   case [ (Var x + fromInteger i) * (Var x + fromInteger j) | (i, j) <- factors c, i+j == b ] of
      hd:_ -> return (hd :==: 0)
      _    -> Nothing

factors :: Integer -> [(Integer, Integer)]
factors n = concat [ [(a, b), (negate a, negate b)] | a <- [1..h], let b = n `div` a, a*b == n ]
 where h = floor (sqrt (abs (fromIntegral n)))

distribution :: Rule (OrList (Equation Expr))
distribution = makeSimpleRuleList "distribution" (onceM  f)
 where
   g = somewhereM (apply distributionT)
   f (lhs :==: rhs) = 
      case (g lhs, g rhs) of
         (Just new, _) -> return (new :==: rhs)
         (_, Just new) -> return (lhs :==: new)
         _             -> [] 

distributionSquare :: Rule (OrList (Equation Expr))
distributionSquare = makeSimpleRule "distribution square" ((onceM  . onceM ) (somewhereM f))
 where
   f (Sym s [x, Nat 2]) | s == powerSymbol = do
      (x, a, b) <- match (linearViewWith rationalView) x
      guard (a /= 0 && (a /= 1 || b /=0))
      return  (  (fromRational (a*a) .*. (Var x^2)) 
             .+. (fromRational (2*a*b) .*. Var x)
             .+. (fromRational (b*b)))
   f _ = Nothing

mergeR :: Rule (OrList (Equation Expr))
mergeR = makeSimpleRuleList "merge" (onceM  (applyAll merge))

simplerA :: Rule (OrList (Equation Expr))
simplerA = makeSimpleRule "simpler A" $ (onceM  f)
 where
   f (lhs :==: rhs) = do
      guard (rhs == 0)
      (x, ra, rb, rc) <- match (quadraticViewWith rationalView) lhs
      [a, b, c] <- mapM isInt [ra, rb, rc] 
      let d = a `gcd` b `gcd` c
      guard (d `notElem` [0, 1])
      return (build quadraticView (x, fromInteger (a `div` d), fromInteger (b `div` d), fromInteger (c `div` d)) :==: 0)

abcFormula :: Rule (OrList (Equation Expr))
abcFormula = makeSimpleRule "abc formula" $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, a, b, c) <- match (quadraticViewWith rationalView) lhs
   let discr = makeSqrt (fromRational (b*b - 4 * a * c))
   -- case discr of Nat n -> guard (even n); _ -> return () -- no nice numbers (for now)
   return $ OrList
      [ Var x :==: (-fromRational b + discr) / (2 * fromRational a)
      , Var x :==: (-fromRational b - discr) / (2 * fromRational a)
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
               (x,y):_ -> error $ show (x, y, match qView x, match qView y) -- (simplify qView x) ++ "    is not    " ++ show (simplify qView y)

main :: IO ()
main = printDerivations quadraticEquationExercise xs 
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
      (_, poly) <- match polyView (lhs - rhs)
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

