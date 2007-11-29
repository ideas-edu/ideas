module Exercises where

import Transformation
import Matrix (Matrix, makeMatrix, rows, columns)
import Strategy
import Utils
import Matrix.Equation
import Matrix.LinearExpr
import Matrix.LinearSystem

import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Test.QuickCheck
   
main = do
   quickCheck propSolvedForm
   quickCheck propSound
   quickCheck propConv1
   quickCheck propConv2

(===) :: Fractional a => LinearSystem a -> LinearSystem a -> Bool
x === y = f x == f y
 where f = fmap (map snd). getSolution . equations . applyD toGeneralSolution . inContext

propConv1 :: LinearSystem Int -> Bool
propConv1 x = matrixToSystem (systemToMatrix y) === y
 where
   y   = map (fmap (renameVariables (fromJust . (`lookup` sub)))) (map (fmap $ fmap toRational) x)
   sub = zip (getVarEquations x) variables

propConv2 :: Matrix Int -> Property
propConv2 x = 
   all (any (/=0))  (columns x)
      ==> systemToMatrix (matrixToSystem x) == x
 
propSolvedForm :: LinearSystem Int -> Bool
propSolvedForm = inSolvedForm  . equations . applyD toGeneralSolution . inContext . (map $ fmap $ fmap toRational)

propSound :: LinearSystem Int -> Property
propSound sys = 
   not (invalidSystem solved) ==> evalSystem (fun $ sub1 ++ sub2) sysRat
 where
   sysRat = map (fmap $ fmap toRational) sys
   solved = filter (not . evalEquation) . equations . applyD toGeneralSolution . inContext $ sysRat
   vars   = map (head . getVars . getLHS) solved
   frees  = nub (getVarEquations sys ++ getVarEquations solved) \\ vars
   sub1   = zip frees (drop 10 (map fromIntegral primes))
   values = map (evalLinearExpr (fun sub1) . getRHS) solved
   sub2   = zip vars values
   fun s  = fromJust . (`lookup` s)

-- elementary equations operations
ruleExchange :: Int -> Int -> Rule [a]
ruleExchange i j = makeRule "Exchange" (f i j)
 where
   f i j xs
      | i == j    = Just xs
      | i >  j    = f j i xs
      | otherwise =
           let (begin, x:rest) = splitAt i xs
               (middle, y:end) = splitAt (j-i-1) rest
           in Just (begin++[y]++middle++[x]++end)
      
ruleEqAdd :: Num a => Int -> Int -> a -> Rule (LinearSystem a)
ruleEqAdd i j a = makeRule "EqAdd" f
 where
   f xs =
      let (begin, this:end) = splitAt i xs
          exprj = xs!!j
      in Just (begin++[combineWith (+) this (fmap (toLinearExpr a*) exprj)]++end)

ruleStandardForm :: Num a => Rule (Equation (LinearExpr a))
ruleStandardForm = makeRule "StandardForm" f
 where
   f eq = do 
      guard (not (inStandardForm eq))
      return (toStandardForm eq)

ruleSubVar :: Fractional a => Rule (LinearSystem a)
ruleSubVar = makeRule "SubVar" f 
 where
   f xs = case [ i | i <- [0 .. length xs-1], applicable (ruleSubVarIndex i) xs ] of
             hd:_ -> apply (ruleSubVarIndex hd) xs
             _    -> Nothing

-- split rule in two?
ruleSubVarIndex :: Fractional a => Int -> Rule (LinearSystem a)
ruleSubVarIndex i = makeRule "ruleSubVarIndex" f
 where
   f xs = 
      let (before, (lhs :==: rhs):after) = splitAt i xs
      in case getVars lhs of
            [v] | getConstant lhs == 0 && (v `elem` getVarEquations (before++after) || con/=1) -> 
               Just $ subst before ++ [new] ++ subst after
             where 
               con    = coefficientOf v lhs
               newrhs = toLinearExpr (1/con) * rhs
               new    = var v :==: newrhs
               subst  = subVarEquations v newrhs
            _ -> Nothing

ruleFreeVarsToRight :: Num a => Rule (LinearSystem a)
ruleFreeVarsToRight = makeRule "FreeVarsToRight" f
 where
   f xs = 
      let vars = [ head ys | ys <- map (getVars . getLHS) xs, not (null ys) ]
          change eq =
             let (e1, e2) = splitLinearExpr (`notElem` vars) (getLHS eq) -- constant ends up in e1
             in e2 :==: getRHS eq - e1
      in Just (map change xs)

-- toEchelon strategy
toGeneralSolution :: Fractional a => Strategy (EqsInContext a)
toGeneralSolution = toStandardFormS <*> toEchelon <*> liftRule ruleFreeVarsToRight <*> echelonToSolution

echelonToSolution :: Fractional a => Strategy (EqsInContext a)
echelonToSolution = repeatS (liftRule ruleSubVar <*> liftRule ruleFreeVarsToRight)

toEchelon :: Fractional a => Strategy (EqsInContext a)
toEchelon = repeatS (firstVarNonZero <*> rEqExchange <*> eliminateVarBelow <*> ruleCoverTop)

toStandardFormS :: Fractional a => Strategy (EqsInContext a)
toStandardFormS = repeatS (liftRule $ liftRuleToList ruleStandardForm)

firstVarNonZero :: Rule (EqsInContext a)
firstVarNonZero = makeRule "_FirstVarNonZero" $ \c -> do
    let remaining_equations  =  drop (covered c) (equations c)
    let minVar               =  minimum (getVarEquations remaining_equations)
    i                        <- findIndex ((minVar `elem`) . getVarEquation) remaining_equations
    return c {minvar = minVar, eqnr = covered c + i}

eliminateVarBelow :: Fractional a => Strategy (EqsInContext a)
eliminateVarBelow  = repeatS (eliminateVar <*> rEqAdd)

eliminateVar :: Fractional a => Rule (EqsInContext a)
eliminateVar = makeRule "_EliminateVar" $ \c -> do
   let coef = coefficientOf (minvar c) (getLHS (equations c !! cureq c))
   let remaining_equations = drop (covered c + 1) (equations c)
   let coefsBelow = map (coefficientOf (minvar c) . getLHS) remaining_equations
   i <- findIndex (/= 0) coefsBelow
   let v = negate (coefficientOf (minvar c) (getLHS (remaining_equations !! i))) / coef
   return c {eqnr = i + covered c + 1, value = v}

ruleCoverTop :: Fractional a => Rule (EqsInContext a)
ruleCoverTop = makeRule "CoverTop" f
 where
   f c = return c {cureq = cureq c + 1,covered = covered c + 1}

-- Context for toEchelon strategy

data EqsInContext a = EIC
   { equations  ::  LinearSystem a
   , minvar     ::  String
   , covered    ::  Int
   , cureq      ::  Int
   , eqnr       ::  Int 
   , value      ::  a
   }
 deriving Show

inContext :: Num a => LinearSystem a -> EqsInContext a
inContext e = EIC e "" 0 0 0 0

instance Eq a => Eq (EqsInContext a) where
  x==y = equations x == equations y

rEqExchange :: Fractional a => Rule (EqsInContext a)
rEqExchange = makeRule "EqExchange" $ selectArgs $ \c -> 
   ruleExchange (cureq c) (eqnr c)

rEqAdd :: Fractional a => Rule (EqsInContext a)
rEqAdd = makeRule "EqAdd" $ selectArgs $ \c -> 
    ruleEqAdd (eqnr c) (cureq c) (value c)

selectArgs :: (EqsInContext a -> Rule (LinearSystem a)) -> EqsInContext a -> Maybe (EqsInContext a)
selectArgs f c = do
   new <- apply (f c) (equations c)
   return c {equations = new}

liftRuleToList :: Rule a -> Rule [a]
liftRuleToList r = makeRule (name r) f 
 where
   f xs = let rs = map (`ruleOnIndex` r) [0 .. length xs-1]
          in apply (combineRules rs) xs

ruleOnIndex :: Int -> Rule a -> Rule [a]
ruleOnIndex i = liftRuleG (LP get set) 
 where
   get     = safeHead . drop i 
   set new = zipWith (\j -> if i==j then const new else id) [0..]

liftRule :: Rule (LinearSystem a) -> Rule (EqsInContext a)
liftRule = liftRuleG $ LP get set
 where
   get = return . equations
   set new c = c {equations = new}

      
liftRuleG :: LiftPair a b -> Rule a -> Rule b
liftRuleG lp r = makeRule (name r) f  
 where
   f x = do
      this <- getter lp x
      new  <- apply r this
      return (setter lp new x)

data LiftPair a b = LP { getter :: b -> Maybe a, setter :: a -> b -> b }

-----------------------------------------------------
-- 9.1 Oplossen van lineaire vergelijkingen
{-
testStep1 = applyD firstVarNonZero (inContext ex1b)
testStep2 = applyD (getRule "EqExchange") testStep1
testStep3 = applyD eliminateVarBelow testStep2
testStep4 = applyD (getRule "CoverTop") testStep3
testStep5 = applyD firstVarNonZero testStep4
testStep6 = applyD (getRule "EqExchange") testStep5
testStep7 = applyD  eliminateVarBelow testStep6
testStep8 = applyD (getRule "CoverTop") testStep7
testStep9 = applyD firstVarNonZero testStep8
testStep10 = applyD (getRule "EqExchange") testStep9
testStep11 = applyD  eliminateVarBelow testStep10

testStrategy1 = applyD toEchelon (inContext ex1a)
ex1atest = testStrategy1 == inContext ex1asolution

testStrategy2 = applyD toEchelon (inContext ex1b)
ex1btest = testStrategy2 == inContext ex1bsolution

testStrategy3 = applyD toEchelon (inContext ex2a)

q = equations $ applyD toGeneralSolution (inContext ex1a) -}

default (Rational)

x1 = var "x1"
x2 = var "x2"
x3 = var "x3"
x4 = var "x4"
x5 = var "x5"

ex1a = 
   [   x1 + 2*x2 + 3*x3 - x4   :==:  0 
   , 2*x1 + 3*x2 - x3 + 3*x4   :==:  0
   , 4*x4 + 6*x2 + x3 + 2*x4   :==:  0
   ]

ex1asolution =
   [ x1 + 2*x2 +  3*x3 -   x4   :==:  0 
   ,    -   x2 -  7*x3 + 5*x4   :==:  0
   ,           - 41*x3 +36*x4   :==:  0
   ]        

ex1b = 
   [ 3*x1 +   x2 + 2*x3 -   x4 :==: 0
   , 2*x1 -   x2 +   x3 +   x4 :==: 0
   , 5*x1 + 5*x2 + 4*x3 - 5*x4 :==: 0
   , 2*x1 + 9*x2 + 3*x3 - 9*x4 :==: 0
   ]
       
ex1bsolution =
   [ 3*x1 +        x2 +      2*x3 -       x4  :==: 0
   ,        (-5/3)*x2 + (-1/3)*x3 + (5/3)*x4  :==: 0
   ,                                       0  :==: 0
   ,                                       0  :==: 0
   ]

ex1c = 
   [ x1 - x2 + x3 + 2*x4 :==: 2
   , 2*x1 - 3*x2 - x4    :==: 3
   , x1 - x3 + 7*x4      :==: 3
   ]
   
ex2a =
   [ x2 + 2*x3        :==: 1
   , x1 + 2*x2 + 3*x3 :==: 2
   , 3*x1 + x2 + x3   :==: 3
   ]
   
ex2b =
   [ x1 + x2 + 2*x3 + 3*x4 - 2*x5 :==: 1
   , 2*x1 + 4*x2 - 8*x5           :==: 3
   , -2*x2 + 4*x3 + 6*x4 + 4*x5   :==: 0
   ]
   
ex2c =
   [ x1 + 2*x2           :==: 0
   , x1 + 4*x2 -2*x3     :==: 4
   , 2*x1 + 4*x3         :==: -8
   , 3*x1 + 6*x3         :==: -12
   , -2*x1 - 8*x2 + 4*x3 :==: -8
   ]
   
ex2d =
   [ x1 + x2 - 2*x3     :==: 0
   , 2*x1 + x2 - 3*x3   :==: 0
   , 4*x1 - 2*x2 - 2*x3 :==: 0
   , 6*x1 - x2 - 5*x3   :==: 0
   , 7*x1 - 3*x2 - 4*x3 :==: 1
   ]