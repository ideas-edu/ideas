module Domain.Math.Polynomial.QuadraticEquations
   (quadraticStrategy, qView, cleanUp, q, go, go2, solvedList, quadraticRules) where

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Traversable
import Common.Uniplate (transform, universe)
import Control.Monad
import Common.Utils (safeHead)
import Data.List (sort, nub, (\\))
import Data.Maybe
import Data.Ratio
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList
import Domain.Math.Data.Polynomial
import Domain.Math.ExercisesDWO
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Polynomial.Generators
import Domain.Math.Polynomial.LinearEquations (merge, distribute)
import Domain.Math.Polynomial.Views
import Domain.Math.Simplification (smartConstructors)
import Domain.Math.SquareRoot.Views
import Domain.Math.View.Basic
import Domain.Math.View.Power
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
         ruleOnce2 (ruleSomewhere merge) <|> ruleOnce cancelTerms  <|> ruleOnce2 distribute
         <|> ruleOnce2 (ruleSomewhere distributionSquare) <|> ruleOnce flipEquation 
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
   f1 = transform (simplify powerFactorView)
   f2 = transform (simplify squareRootView)

------------------------------------------------------------
-- Rule collection

quadraticRules :: [Rule (OrList (Equation Expr))]
quadraticRules = 
   [ ruleOnce noConFormula, ruleOnce noLinFormula, ruleOnce niceFactors
   , ruleOnce simplerA, abcFormula, mulZero, coverUpSquare, ruleOnce coverUpPlus
   , ruleOnce coverUpTimes, ruleOnce coverUpNegate, ruleOnce coverUpDiv
   , ruleOnce2 (ruleSomewhere merge), ruleOnce cancelTerms , ruleOnce2 distribute
   , ruleOnce2 (ruleSomewhere distributionSquare), ruleOnce flipEquation 
   , ruleOnce moveToLeft
   ]

------------------------------------------------------------
-- General form rules: ax^2 + bx + c = 0

-- ax^2 + bx = 0 
noConFormula :: Rule (Equation Expr) 
noConFormula = makeSimpleRule "no constant c" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   guard (c == 0 && b /= 0)
   -- also search for constant factor
   let d = gcdFrac a b
   return (fromRational d .*. Var x .*. (fromRational (a/d) .*. Var x .+. fromRational (b/d)) :==: 0)

-- ax^2 + c = 0
noLinFormula :: Rule (Equation Expr)
noLinFormula = makeSimpleRule "no linear term b" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   guard (b == 0 && c /= 0)
   return $ 
      if a>0 then fromRational a .*. (Var x .^. 2) :==: fromRational (-c)
             else fromRational (-a) .*. (Var x .^. 2) :==: fromRational c

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

simplerA :: Rule (Equation Expr)
simplerA = makeSimpleRule "simpler A" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (ra, rb, rc)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   [a, b, c] <- mapM isInt [ra, rb, rc] 
   let d = a `gcd` b `gcd` c
   guard (d `notElem` [0, 1])
   return (build quadraticView (x, fromInteger (a `div` d), fromInteger (b `div` d), fromInteger (c `div` d)) :==: 0)

abcFormula :: Rule (OrList (Equation Expr))
abcFormula = makeSimpleRule "abc formula" $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (x, (a, b, c)) <- match (polyNormalForm rationalView >>> second quadraticPolyView) lhs
   let discr = makeSqrt (fromRational (b*b - 4 * a * c))
   return $ OrList
      [ Var x :==: (-fromRational b + discr) / (2 * fromRational a)
      , Var x :==: (-fromRational b - discr) / (2 * fromRational a)
      ]

------------------------------------------------------------
-- General form rules: expr = 0

mulZero :: Rule (OrList (Equation Expr))
mulZero = makeSimpleRule "multiplication is zero" $ onceJoinM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (_, xs) <- match productView lhs
   guard (length xs > 1)
   return (OrList [ x :==: 0 | x <- xs ])

------------------------------------------------------------
-- Constant form rules: expr = constant

coverUpSquare = coverUpPower
coverUpDiv    = coverUpDenominator
coverUpPlus   = coverUpPlusWith "special" p
 where p x = hasVars x && maybe False ((==1) . length) (match sumView x) 

------------------------------------------------------------
-- Top form rules: expr1 = expr2

cancelTerms :: Rule (Equation Expr)
cancelTerms = makeSimpleRule "cancel terms" $ \(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   let zs = filter (`elem` ys) (nub xs)
   guard (not (null zs))
   let without as = build sumView (as \\ zs)
   return (without xs :==: without ys)

distributionSquare :: Rule Expr
distributionSquare = makeSimpleRule "distribution square" f
 where
   f (Sym s [x, Nat 2]) | s == powerSymbol = do
      (x, a, b) <- match (linearViewWith rationalView) x
      guard (a /= 0 && (a /= 1 || b /=0))
      return  (  (fromRational (a*a) .*. (Var x^2)) 
             .+. (fromRational (2*a*b) .*. Var x)
             .+. (fromRational (b*b))
              )
   f _ = Nothing

flipEquation :: Rule (Equation Expr)
flipEquation = makeSimpleRule "flip equation" $ \(lhs :==: rhs) -> do
   guard (hasVars rhs && noVars lhs)
   return (rhs :==: lhs)

moveToLeft :: Rule (Equation Expr)
moveToLeft = makeSimpleRule "move to left" $ \(lhs :==: rhs) -> do
   guard (rhs /= 0)
   let complex = case fmap (filter hasVars) $ match sumView (applyD merge lhs) of
                    Just xs | length xs >= 2 -> True
                    _ -> False
   guard (hasVars lhs && (hasVars rhs || complex))
   return (lhs - rhs :==: 0)

------------------------------------------------------------
-- Helpers and Rest

makeSqrt :: Expr -> Expr
makeSqrt (Nat n) | a*a == n = Nat a
 where a = SQ.isqrt n
makeSqrt e = sqrt e

factors :: Integer -> [(Integer, Integer)]
factors n = concat [ [(a, b), (negate a, negate b)] | a <- [1..h], let b = n `div` a, a*b == n ]
 where h = floor (sqrt (abs (fromIntegral n)))

isInt :: Rational -> Maybe Integer
isInt r = do
   guard (denominator r == 1)
   return (numerator r)

------------------------------------------------------------
-- Testing


go = mapM_ f $ zip [0..] (concat quadraticEquations)
 where 
   f (n, eq) = 
      let start  = OrList [eq]
          OrList result = applyD quadraticStrategy start
          p (x :==: y) = x == Var "x" && y `belongsTo` squareRootView
      in if all p result then putStr (show n++" ok; ") else error $ show result ++ " for " ++ show n

go2 = quickCheck $ 
   forAll (sized quadraticGen) $ \a -> 
   forAll (sized quadraticGen) $ \b -> 
   let start  = OrList [a :==: b]
       OrList result = applyD quadraticStrategy start
       p (x :==: y) = x == Var "x" && y `belongsTo` squareRootView
   in if all p result then True else error $ "go2: " ++ show result
    
go3 = quickCheck $ 
   forAll (sized linearGen) $ \a -> 
   forAll (sized linearGen) $ \b -> 
   let start  = OrList [a :==: b]
       OrList result = applyD quadraticStrategy start
       p (x :==: y) = x == Var "x" && y `belongsTo` squareRootView
   in if all p result then True else error $ "go2: " ++ show result
   
gcdFrac :: Rational -> Rational -> Rational
gcdFrac r1 r2 = fromMaybe 1 $ do 
   a <- isInt r1
   b <- isInt r2
   return (fromInteger (gcd a b))
      

   
q = putStrLn $ showDerivationWith show (ignoreContext $ unlabel quadraticStrategy) $ 
   let x=Var "x" in OrList $ return $ 
   --concat quadraticEquations !! 45 -- quadraticEquations !! 35
   
   237720469701/7115275300*x^2 :==: -2735/216663796*x^2+23536855
   /3899948328*x-50020/31116609-(-49/8*x+(164/9-x)*(1/11))*(40832/17201)*x
   
   -- *** Exception: Cleaning-up: (-7/3 == x/(10/7)+881/1672,
                         --          -7/3 == 7/10*x+881/1672)
              
allSame :: Eq a => [a] -> Bool           
allSame []     = True
allSame (x:xs) = all (==x) xs                         

qView :: View (OrList (Equation Expr)) (String, [SQ.SquareRoot Rational])
qView = makeView f g
 where
   f (OrList xs) = do 
      ps <- mapM (match qView2) xs
      let (ss, xss) = unzip ps
          pv        = fromMaybe "" (safeHead ss)
          make      = sort . nub . filter (not . SQ.imaginary) . concat
      guard (allSame ss)
      return (pv, make xss)
   g (s, xs) = OrList [ Var s :==: build squareRootView rhs | rhs <- xs ]

qView2 :: View (Equation Expr) (String, [SQ.SquareRoot Rational])
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
            return (x, [SQ.scale (-1/rb) rc])
         False -> do 
            rc <- match rationalView (coefficient 0 poly)
            let discr = rb*rb - 4*ra*rc
            case compare discr 0 of
               LT -> Just (x, [])
               EQ -> Just (x, [SQ.con (-rb/(2*ra))])
               GT ->  
                  let sdiscr = SQ.sqrtRational discr
                  in return (x, [ SQ.scale (1/(2*ra)) (-SQ.con rb + sdiscr)
                                , SQ.scale (1/(2*ra)) (-SQ.con rb - sdiscr)
                                ])
                            
solvedList :: OrList (Equation Expr) -> Bool
solvedList (OrList xs) = all (`belongsTo` equationSolvedForm) xs