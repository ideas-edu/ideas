module Domain.Math.Polynomial.QuadraticEquations
   (quadraticStrategy, qView, cleanUp, q, go, go2, solvedList, quadraticRules) where

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Traversable
import Common.Uniplate (somewhereM, transform, universe)
import Control.Monad
import Data.List hiding (repeat)
import Data.Maybe
import Data.Ratio
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList
import Domain.Math.ExercisesDWO
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Simplification (smartConstructors)
import Domain.Math.Polynomial.LinearEquations (mergeT, distributionT, solvedEquation)
import Domain.Math.View.Basic hiding (linearView)
import Domain.Math.Polynomial.Views hiding (quadraticView)
import Domain.Math.Numeric.Generators
import Domain.Math.View.SquareRoot
import Prelude hiding (repeat, (^), replicate)
import Test.QuickCheck hiding (label)
import qualified Domain.Math.Data.SquareRoot as SQ
import qualified Prelude
import qualified Test.QuickCheck as QC

------------------------------------------------------------
-- Strategy

quadraticStrategy :: LabeledStrategy (OrList (Equation Expr))
quadraticStrategy = cleanUpStrategy cleanUp $ 
   label "Quadratic Equation Strategy" $ 
   repeat $
         -- general form
      (  label "general form" $ 
         ( ruleOnce noConFormula <|> ruleOnce noLinFormula 
           <|> ruleOnce niceFactors <|> ruleOnce simplerA )
         |> abcFormula
      )
      |> -- zero form
      (  label "zero form" $ 
         mulZero
      )
      |> -- constant form
      (  label "constant form" $ 
         coverUpSquare <|> ruleOnce coverUpPlus <|> ruleOnce coverUpTimes
         <|> ruleOnce coverUpNegate <|> ruleOnce coverUpDiv
      )
      |> -- top form
      (  label "top form" $ 
         ruleOnce2 merge2 <|> ruleOnce cancelTerms  <|> ruleOnce2 distribute2
         <|> ruleOnce2 distributionSquare <|> ruleOnce flipEquation 
         <|> ruleOnce moveToLeft
      )

------------------------------------------------------------
-- Cleaning up

cleanUp :: OrList (Equation Expr) -> OrList (Equation Expr)
cleanUp (OrList xs) = OrList (filter keepEquation (map (fmap cleanUpExpr) xs))

keepEquation :: Equation Expr -> Bool
keepEquation (a :==: b) = not (trivial || any falsity (universe a ++ universe b))  
 where
   trivial = noVars a && noVars b
   falsity (Sqrt e)  = maybe False (<0)  (match rationalView e)
   falsity (_ :/: e) = maybe False (==0) (match rationalView e)
   falsity _         = False

cleanUpExpr :: Expr -> Expr
cleanUpExpr = smartConstructors . f2 . f1 . smartConstructors . simplify sumView
 where
   f1 = transform (simplify powerView)
   f2 = transform (simplify squareRootView)

------------------------------------------------------------
-- Rule collection

quadraticRules :: [Rule (OrList (Equation Expr))]
quadraticRules = 
   [ ruleOnce noConFormula, ruleOnce noLinFormula, ruleOnce niceFactors
   , ruleOnce simplerA, abcFormula, mulZero, coverUpSquare, ruleOnce coverUpPlus
   , ruleOnce coverUpTimes, ruleOnce coverUpNegate, ruleOnce coverUpDiv
   , ruleOnce2 merge2, ruleOnce cancelTerms , ruleOnce2 distribute2
   , ruleOnce2 distributionSquare, ruleOnce flipEquation 
   , ruleOnce moveToLeft
   ]

------------------------------------------------------------
-- Rules

makeSqrt :: Expr -> Expr
makeSqrt (Nat n) | a*a == n = Nat a
 where a = SQ.isqrt n
makeSqrt e = sqrt e

-- x^2 = A
coverUpSquare :: Rule (OrList (Equation Expr))
coverUpSquare = makeSimpleRule "constant square" (onceJoinM f) 
 where
   f (Sym s [e, Nat 2] :==: rhs) | s == powerSymbol = do
      r <- match rationalView rhs
      let s = makeSqrt rhs
      case compare r 0 of
         LT -> return $ OrList []
         EQ -> return $ OrList [e :==: 0]
         GT -> return $ OrList [e :==: s, e :==: negate s]
   f _ = Nothing

isLinear :: Expr -> Bool
isLinear e = e `belongsTo` (polyNormalForm rationalView >>> second linearPolyView)

coverUpPlus :: Rule (Equation Expr)
coverUpPlus = makeSimpleRule "cover-up plus/minus" $ \(lhs :==: rhs) -> do
   guard (noVars rhs)
   (e1, e2) <- case match sumView lhs of
                  Just [e1, e2] -> return (e1, e2)
                  _             -> Nothing
   guard (rhs /= 0 || isLinear lhs)
   case (match rationalView e1, match rationalView e2) of
      (Just a, Nothing) -> return (e2 :==: rhs - fromRational a)
      (Nothing, Just a) -> return (e1 :==: rhs - fromRational a)
      _ -> Nothing

coverUpTimes :: Rule (Equation Expr)
coverUpTimes = makeSimpleRule "cover-up times" $ \(lhs :==: rhs) -> do
   guard (noVars rhs)
   (e1, e2) <- match timesView lhs
   case (match rationalView e1, match rationalView e2) of
      (Just a, Nothing) | a /= 0 -> return (e2 :==: rhs/fromRational a)
      (Nothing, Just a) | a /= 0 -> return (e1 :==: rhs/fromRational a)
      _ -> Nothing

coverUpNegate :: Rule (Equation Expr)
coverUpNegate = makeSimpleRule "cover-up negate" f
 where
   f (Negate a :==: b) = do
      guard (noVars b)
      return (a :==: -b)
   f _ = Nothing

coverUpDiv :: Rule (Equation Expr)
coverUpDiv = makeSimpleRule "cover-up division" $ \(lhs :==: rhs) -> do
   b <- match rationalView rhs
   (e1, e2) <- match divView lhs
   a <- match rationalView e2
   guard (a /= 0)
   return (e1 :==: fromRational (b*a))

cancelTerms :: Rule (Equation Expr)
cancelTerms = makeSimpleRule "cancel terms" $ \(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   let zs = filter (`elem` ys) (nub xs)
   guard (not (null zs))
   let without as = build sumView (as \\ zs)
   return (without xs :==: without ys)

{-
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
   return (OrList [ Var x1 :==: 0, make e1 n1 .+. make e2 n2 :==: 0 ]) -}

mulZero :: Rule (OrList (Equation Expr))
mulZero = makeSimpleRule "multiplication is zero" $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (_, xs) <- match productView lhs
   guard (length xs > 1)
   return (OrList [ x :==: 0 | x <- xs ])
   
moveToLeft :: Rule (Equation Expr)
moveToLeft = makeSimpleRule "move to left" $ \(lhs :==: rhs) -> do
   guard (rhs /= 0)
   let complex = case fmap (filter hasVars) $ match sumView (applyD mergeT lhs) of
                    Just xs | length xs >= 2 -> True
                    _ -> False
   guard (hasVars lhs && (hasVars rhs || complex))
   return (lhs - rhs :==: 0)

-- search for (X+A)*(X+B) decomposition 
niceFactors :: Rule (Equation Expr)
niceFactors = makeSimpleRule "nice factors" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   let sign t@(x, (a, b, c)) = if a== -1 then (x, (1, -b, -c)) else t 
   (x, (a, rb, rc)) <- liftM sign (match (polyNormalForm rationalView >>> second quadraticPolyView) lhs)
   guard (a==1)
   b <- isInt rb
   c <- isInt rc
   case [ (Var x + fromInteger i) * (Var x + fromInteger j) | (i, j) <- factors c, i+j == b ] of
      hd:_ -> return (hd :==: 0)
      _    -> Nothing

factors :: Integer -> [(Integer, Integer)]
factors n = concat [ [(a, b), (negate a, negate b)] | a <- [1..h], let b = n `div` a, a*b == n ]
 where h = floor (sqrt (abs (fromIntegral n)))
 
distributionSquare :: Rule Expr
distributionSquare = makeSimpleRule "distribution square" (somewhereM f)
 where
   f (Sym s [x, Nat 2]) | s == powerSymbol = do
      (x, a, b) <- match (linearViewWith rationalView) x
      guard (a /= 0 && (a /= 1 || b /=0))
      return  (  (fromRational (a*a) .*. (Var x^2)) 
             .+. (fromRational (2*a*b) .*. Var x)
             .+. (fromRational (b*b))
              )
   f _ = Nothing

simplerA :: Rule (Equation Expr)
simplerA = makeSimpleRule "simpler A" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, ra, rb, rc) <- match quadraticView lhs
   [a, b, c] <- mapM isInt [ra, rb, rc] 
   let d = a `gcd` b `gcd` c
   guard (d `notElem` [0, 1])
   return (build quadraticView (x, fromInteger (a `div` d), fromInteger (b `div` d), fromInteger (c `div` d)) :==: 0)

abcFormula :: Rule (OrList (Equation Expr))
abcFormula = makeSimpleRule "abc formula" $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   let discr = makeSqrt (fromRational (b*b - 4 * a * c))
   -- case discr of Nat n -> guard (even n); _ -> return () -- no nice numbers (for now)
   return $ OrList
      [ Var x :==: (-fromRational b + discr) / (2 * fromRational a)
      , Var x :==: (-fromRational b - discr) / (2 * fromRational a)
      ]

flipEquation :: Rule (Equation Expr)
flipEquation = makeSimpleRule "flip equation" $ \(lhs :==: rhs) -> do
   guard (hasVars rhs && noVars lhs)
   return (rhs :==: lhs)

isInt :: Rational -> Maybe Integer
isInt r = do
   guard (denominator r == 1)
   return (numerator r)

------------------------------------------------------------
-- Testing

------------------------------------------------------------
-- QuadGen

quadGen :: Int -> Gen Expr
quadGen = symbolGenerator extras syms
 where
   syms = [plusSymbol, minusSymbol, negateSymbol]
   extras n = [ ratGen, varGenerator ["x"] ] ++ 
              [ g | n > 0, g <- [timesGen1, timesGen2, divGen, sqGen] ]
    where 
      h = n `div` 2
      timesGen1 = oneof
         [ liftM2 (+) (quadGen h) ratGenNZ
         , liftM2 (+) ratGenNZ (quadGen h)
         ]
      timesGen2 = liftM2 (+) (linGen h) (linGen h)
      divGen    = liftM2 (/) (quadGen h) ratGenNZ
      sqGen     = liftM (^ 2) (linGen h)
   
linGen :: Int -> Gen Expr
linGen = symbolGenerator extras syms
 where
   syms = [plusSymbol, minusSymbol, negateSymbol]
   extras n = [ ratGen, varGenerator ["x"] ] ++ 
              [ g | n > 0, g <- [timesGen, divGen] ]
    where 
      h = n `div` 2
      timesGen = oneof
         [ liftM2 (+) (linGen h) ratGenNZ
         , liftM2 (+) ratGenNZ (linGen h)
         ]
      divGen    = liftM2 (/) (linGen h) ratGenNZ

ratGen, ratGenNZ :: Gen Expr
ratGen   = liftM fromRational (ratioGen 40 10)
ratGenNZ = liftM fromRational (ratioGenNonZero 40 10)

go = mapM_ f $ zip [0..] (concat quadraticEquations)
 where 
   f (n, eq) = 
      let start  = OrList [eq]
          OrList result = applyD quadraticStrategy start
          p (x :==: y) = x == Var "x" && y `belongsTo` squareRootView
      in if all p result then putStrLn (show n++" ok") else error $ show result ++ " for " ++ show n

go2 = quickCheck $ 
   forAll (sized quadGen) $ \a -> 
   forAll (sized quadGen) $ \b -> 
   let start  = OrList [a :==: b]
       OrList result = applyD quadraticStrategy start
       p (x :==: y) = x == Var "x" && y `belongsTo` squareRootView
   in if all p result then True else error $ "go2: " ++ show result
    





{- linFormula :: Rule (Equation Expr)
linFormula = makeSimpleRule "linear formula" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, a, b) <- match (linearViewWith rationalView) lhs
   guard (a /= 0 && (a /= 1 || b /= 0))
   return (Var "x" :==: fromRational (-b/a)) -}
   
noConFormula :: Rule (Equation Expr) -- ax^2 + bx
noConFormula = makeSimpleRule "no constant c" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   guard (c == 0 && b /= 0)
   -- also search for constant factor
   let d = gcdFrac a b
   return (fromRational d .*. Var x .*. (fromRational (a/d) .*. Var x .+. fromRational (b/d)) :==: 0)
   
gcdFrac :: Rational -> Rational -> Rational
gcdFrac r1 r2 = fromMaybe 1 $ do 
   a <- isInt r1
   b <- isInt r2
   return (fromInteger (gcd a b))
   
noLinFormula :: Rule (Equation Expr) -- ax^2 + c
noLinFormula = makeSimpleRule "no linear term b" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   guard (b == 0 && c /= 0)
   return $ 
      if a>0 then fromRational a .*. (Var x .^. 2) :==: fromRational (-c)
             else fromRational (-a) .*. (Var x .^. 2) :==: fromRational c
   
merge2 :: Rule Expr
merge2 = makeSimpleRuleList "merge" $ somewhereM $ \a -> do
   b <- applyAll mergeT a
   guard (a /= b)
   return b

distribute2 :: Rule Expr
distribute2 = makeSimpleRuleList "distribute" $ somewhereM $ \a -> do
   b <- applyAll distributionT a
   guard (a /= b)
   return b
   
q = putStrLn $ showDerivationWith show (ignoreContext $ unlabel quadraticStrategy) $ 
   let x=Var "x" in OrList $ return $ 
   concat quadraticEquations !! 35
   
   -- *** Exception: Cleaning-up: (-7/3 == x/(10/7)+881/1672,
                         --          -7/3 == 7/10*x+881/1672)
                         
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
                            
solvedList :: OrList (Equation Expr) -> Bool
solvedList (OrList xs) = all solvedEquation xs