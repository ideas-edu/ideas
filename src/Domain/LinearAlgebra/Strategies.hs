-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Strategies where

import Prelude hiding (repeat)
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.EquationsRules
import Domain.LinearAlgebra.GramSchmidtRules
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.LinearExpr
import Common.Strategy hiding (not)
import Common.Transformation
import Common.Context
import Domain.LinearAlgebra.Vector

toReducedEchelon :: (Argument a, Fractional a) => LabeledStrategy (Context (Matrix a))
toReducedEchelon = label "Gaussian elimination" $ 
   forwardPass <*> backwardPass

forwardPass :: (Argument a, Fractional a) => LabeledStrategy (Context (Matrix a))
forwardPass = label "Forward pass" $ 
   repeat  $    label "Find j-th column"      ruleFindColumnJ 
           <*>  label "Exchange rows"         (try ruleExchangeNonZero)
           <*>  label "Scale row"             (try ruleScaleToOne)
           <*>  label "Zeros in j-th column"  (repeat ruleZerosFP)
           <*>  label "Cover up top row"      ruleCoverRow
  
backwardPass :: (Argument a, Fractional a) => LabeledStrategy (Context (Matrix a))
backwardPass =  label "Backward pass" $ 
   repeat  $    label "Uncover row"  ruleUncoverRow
           <*>  label "Sweep"        (repeat ruleZerosBP)

backSubstitutionSimple :: (Argument a, IsLinear a) => LabeledStrategy (Context (LinearSystem a))
backSubstitutionSimple = label "Back substitution with equally many variables and equations" $
       label "Cover all equations" ruleCoverAllEquations
   <*> repeat (   label "Uncover one equation"  ruleUncoverEquation
              <*> label "Scale equation to one" (try ruleScaleEquation)
              <*> label "Back Substitution"     (repeat ruleBackSubstitution)
              )

backSubstitution :: (Argument a, IsLinear a) => LabeledStrategy (Context (LinearSystem a))
backSubstitution = label "Back substitution" $ 
   ruleIdentifyFreeVariables <*> backSubstitutionSimple
   
systemToEchelonWithEEO :: (Argument a, IsLinear a) => LabeledStrategy (Context (LinearSystem a))
systemToEchelonWithEEO = label "System to Echelon Form (EEO)" $
   repeat $   label "Inconsistent system (0=1)" ruleInconsistentSystem
          <|> label "Drop (0=0) equation"       ruleDropEquation
          <|> check (not . null . remaining)
          <*> label "Exchange equations"        (try ruleExchangeEquations)
          <*> label "Scale equation to one"     (option ruleScaleEquation)
          <*> label "Eliminate variable"        (repeat ruleEliminateVar)
          <*> label "Cover up first equation"   ruleCoverUpEquation

generalSolutionLinearSystem :: (Argument a, IsLinear a) => LabeledStrategy (Context (LinearSystem a))
generalSolutionLinearSystem = label "General solution to a linear system" $
   systemToEchelonWithEEO <*> backSubstitution


generalSolutionSystemWithMatrix :: (Argument a, IsLinear a) => LabeledStrategy (Context (Either (LinearSystem a) (Matrix a)))
generalSolutionSystemWithMatrix = label "General solution to a linear system (matrix approach)" $
   conv1 <*> liftRight toReducedEchelon <*> conv2

gramSchmidt :: Floating a => LabeledStrategy (Context [Vector a])
gramSchmidt = label "Gram-Schmidt" $ repeat $ label "Iteration" $
       label "Consider next vector"   ruleNext 
   <*> label "Make vector orthogonal" (repeat (ruleOrthogonal <*> ruleNextOrthogonal)) 
   <*> label "Normalize"              (try ruleNormalize)

vars :: Var [String]
vars = "variables" := []

conv1 :: IsLinear a => Rule (Context (Either (LinearSystem a) (Matrix a)))
conv1 = translationToContext "Linear system to matrix" $ \c -> 
   let (m, vs) = systemToMatrix (fromContext c)
   in return $ set vars vs $ fmap (const m) c
 
conv2 :: IsLinear a => Rule (Context (Either (LinearSystem a) (Matrix a)))
conv2 = translationFromContext "Matrix to linear system" $ \c -> 
   let linsys = matrixToSystemWith (get vars c) (fromContext c)
   in return $ fmap (const linsys) c 
   
liftLeft :: LabeledStrategy (Context a) -> LabeledStrategy (Context (Either a b))
liftLeft = lift $
   makeLiftPair (maybeInContext . fmap isLeft) (\a _ -> fmap Left a)

liftRight :: LabeledStrategy (Context b) -> LabeledStrategy (Context (Either a b))
liftRight = lift $ 
   makeLiftPair (maybeInContext . fmap isRight) (\b _ -> fmap Right b)

maybeInContext :: Context (Maybe a) -> Maybe (Context a)
maybeInContext c = fmap (\a -> fmap (const a) c) (fromContext c)

isLeft :: Either a b -> Maybe a
isLeft = either Just (const Nothing)

isRight :: Either a b -> Maybe b
isRight = either (const Nothing) Just

translationTo :: String -> (a -> Maybe b) -> Rule (Either a b)
translationTo s f = makeSimpleRule s (either (fmap Right . f) (const Nothing))

translationFrom :: String -> (b -> Maybe a) -> Rule (Either a b)
translationFrom s f = makeSimpleRule s (either (const Nothing) (fmap Left . f))

translationToContext :: String -> (Context a -> Maybe (Context b)) -> Rule (Context (Either a b))
translationToContext s f = makeSimpleRule s (maybe Nothing (fmap (fmap Right) . f) . maybeInContext . fmap isLeft)

-- earlier version, with a slightly differnt type
--translationToContext s f = makeSimpleRule s (maybeInContext . fmap (fmap Right . maybe Nothing f . isLeft))

translationFromContext :: String -> (Context b -> Maybe (Context a)) -> Rule (Context (Either a b))
translationFromContext s f = makeSimpleRule s (maybe Nothing (fmap (fmap Left) . f) . maybeInContext . fmap isRight)

--translationFromInContext :: String -> (b -> Maybe a) -> Rule (Context (Either a b))
--translationFromInContext s f = makeSimpleRule s (maybeInContext . fmap (fmap Left . maybe Nothing f . isRight))


-- temp for testing
-- opgave 9.74
{-
a1, a2, a3 :: Vector MySqrt
a1 = fromList [1,1,1,1]
a2 = fromList [3,3,1,1]
a3 = fromList [7,9,3,5]
a1n = toUnit a1
a2n = toUnit $ makeOrthogonal a1n a2
a3n = toUnit $ makeOrthogonal a2n $ makeOrthogonal a1n a3

testGS = applyAll gramSchmidt $ inContext [a1 , a2, a3]

q = makeOrthogonal (a1n) a2

testGramSchmidt :: Floating a => [Vector a] -> [Vector a]
testGramSchmidt = foldr op []
 where op xs yss = toUnit (foldr makeOrthogonal xs yss) : yss
 
test = applyAll generalSolutionSystemWithMatrix (inContext $ Left  ex1a)

ex1a :: Equations (LinearExpr Rational)
ex1a = 
   [   x1 + 2*x2 + 3*x3 - x4   :==:  0 
   , 2*x1 + 3*x2 - x3 + 3*x4   :==:  0
   , 4*x4 + 6*x2 + x3 + 2*x4   :==:  0
   ]

x1 = var "x1"
x2 = var "x2"
x3 = var "x3"
x4 = var "x4"
x5 = var "x5" -}

-------------------------------------------------------------
-- Square roots
      
-- a/sqrt b = (a/b) * sqrt b
-- a / (n * sqrt b) = a/(n*b)   * sqrt b
{-
data MySqrt = Con Rational | Sqrt Rational Integer | MySqrt :+: MySqrt deriving Show

instance Num MySqrt where
   Con 0 + x = x
   x + Con 0 = x
   Con a + Con b  = Con (a+b)
   Sqrt a n + Sqrt b m | n==m = Sqrt (a+b) n
   -- a + b = a :+: b
   
   a + b = error $ show ("(+)", a, b)
   
   Con 1 * x = x
   x * Con 1 = x
   (a :+: b) * x = a*x :+: b*x
   x * (a :+: b) = x*a :+: x*b
   Con a * Con b  = Con (a*b)
   Con a * Sqrt n b = Sqrt (a*n) b
   Sqrt n b * Con a = Sqrt (a*n) b
   Sqrt n a * Sqrt m b = Con (n*m) * sqrt (Con $ fromIntegral (a*b))
   
   
   
   negate (Con a) = Con (negate a)
   negate (Sqrt r n) = Sqrt (negate r) n
   negate (a :+: b)  = negate a :+: negate b
   fromInteger = fromRational . fromInteger
   abs    = error "abs"
   signum = error "signum"
   
instance Fractional MySqrt where
   recip (Con a)    = Con (recip a)
   recip (Sqrt a n) = Sqrt (recip (fromIntegral n*a)) n
   recip (a :+: b)  = error "recip on a sum"
   fromRational     = Con

instance Floating MySqrt where
   sqrt (Con r)
      | denominator r == 1 =
           toSquareRoot (numerator r)
      | otherwise = 
           Con (recip $ fromIntegral $ denominator r) * toSquareRoot (numerator r * denominator r)
   sqrt a = error $ show ("sqrt", a)

instance Eq MySqrt where
   a == b = fromMySqrt a ~= fromMySqrt b
    where x ~= y = abs (x-y) < 0.0000001

toSquareRoot :: Integer -> MySqrt 
toSquareRoot n
   | n < 0     = negate (toSquareRoot (negate n))
   | n == 0    = Con 0
   | n == 1    = Con 1
   | otherwise = rec 1 1 (factors n)
 where
   rec a b [] 
      | b == 1    = Con a
      | otherwise = Sqrt a b
   rec a b [x] = Sqrt a (x*b)
   rec a b (x:y:zs)
      | x == y    = rec (fromIntegral x*a) b zs
      | otherwise = rec a (x*b) (y:zs)


-- argument should be > 0
factors :: Integer -> [Integer]
factors = smartRec (take 50 primes)
 where
   smartRec list n = 
      case hasSquareRoot n of 
         Just x  -> [x, x] 
         Nothing -> rec list n
 
   rec [] n = [n]
   rec list@(p:ps) n
      | n == 1         = []
      | n `mod` p == 0 = p : smartRec list (n `div` p)
      | 2*p > n        = [n]
      | otherwise      = rec ps n

hasSquareRoot :: Integer -> Maybe Integer
hasSquareRoot n
   | r*r == n  = Just r
   | otherwise = Nothing
 where
   r = round $ sqrt $ fromIntegral n
   
primes :: [Integer]
primes = sieve [2..]
 where
   sieve (x:xs) = x : sieve (filter ((/=0) . (`mod` x)) xs)

fromMySqrt :: MySqrt -> Float
fromMySqrt (Con n)   = fromRational n
fromMySqrt (Sqrt r m)  = fromRational r * sqrt (fromIntegral m)
fromMySqrt (a :+: b) = fromMySqrt a + fromMySqrt b -}