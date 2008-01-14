{-# OPTIONS -XGeneralizedNewtypeDeriving #-}
module OpenMath.Exercises where

import Common.Transformation

import Common.Transformation
import Domain.LinearAlgebra (Matrix, makeMatrix, rows, columns)
import Common.Strategy
import Common.Utils
import Common.Unification
import Common.Assignment
import Domain.LinearAlgebra.Equation
import Domain.LinearAlgebra.LinearExpr
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.Checks ()
import Domain.LinearAlgebra (toReducedEchelon, MatrixInContext, parseSystem)
import qualified Domain.LinearAlgebra.Context as Matrix
import qualified Common.Strategy as Strategy

import qualified Data.Set as S
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Test.QuickCheck
-- import Debug.Trace
import GHC.Real
   
main = do
   quickCheck propSolvedForm
   quickCheck propSound
   quickCheck propConv1
   quickCheck propConv2

equationsAssignment :: Assignment (EqsInContext Rational)
equationsAssignment = makeAssignment
   { shortTitle    = "Solve linear equations"
   , parser        = either (\(x,y) -> Left (x, fmap inContext y)) (Right . inContext) . parseSystem
   , prettyPrinter = showLinSystem . equations
   , equivalence   = \x y -> let f = getSolution . equations . applyD toGeneralSolution 
                             in f x == f y
   , finalProperty = inSolvedForm . equations
   , strategy      = toGeneralSolution
   , generator     = liftM inContext (vector 3)
   }
  
q = checkAssignment equationsAssignment 
   
showLinSystem :: LinearSystem Rational -> String
showLinSystem = unlines . map (show . fmap (fmap MyRational))

newtype MyRational = MyRational Rational
  deriving (Eq, Num)
   
instance Show MyRational where
   show (MyRational (x :% y))
      | y==1      = show x
      | otherwise = show x ++ "/" ++ show y
   
instance (Arbitrary a, Num a) => Arbitrary (EqsInContext a) where
   arbitrary   = liftM inContext arbitrary
   coarbitrary = coarbitrary . equations

(===) :: Fractional a => LinearSystem a -> LinearSystem a -> Bool
x === y = f x == f y
 where f = fmap (map snd). getSolution . equations . applyD toGeneralSolution . inContext

propConv1 :: LinearSystem Int -> Bool
propConv1 x = matrixToSystem (systemToMatrix y) === y
 where
   y   = map (fmap g) (map (fmap $ fmap toRational) x)
   sub = zip (getVarsList x) variables
   g   = renameVariables (fromJust . (`lookup` sub))

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
   vars   = getVars (map getLHS solved)
   frees  = getVars (sys, solved) S.\\ vars
   sub1   = zip (S.toList frees) (drop 10 (map fromIntegral primes))
   sub2   = zip (S.toList vars) values
   values = map (evalLinearExpr (fun sub1) . getRHS) solved
   fun s  = fromJust . (`lookup` s)
    
-- elementary equations operations
exchange :: Int -> Int -> Transformation [a]
exchange i j 
   | i >  j    = exchange j i
   | otherwise = makeTrans $ \xs -> do
        guard (i/=j)
        let (begin, x:rest) = splitAt i xs
            (middle, y:end) = splitAt (j-i-1) rest
        return $ begin++[y]++middle++[x]++end
      
addEquations :: Num a => Int -> Int -> a -> Transformation (LinearSystem a)
addEquations i j a = makeTrans $ \xs -> do
   guard (i/=j)
   let (begin, this:end) = splitAt i xs
       exprj = xs!!j
   return $ begin++[combineWith (+) this (fmap (toLinearExpr a*) exprj)]++end

equationToStandardForm :: Num a => Transformation (Equation (LinearExpr a))
equationToStandardForm = makeTrans $ \eq -> do
   guard (not (inStandardForm eq))
   return (toStandardForm eq)

ruleSubVar :: Fractional a => Rule (LinearSystem a)
ruleSubVar = makeSimpleRule "SubVar" f 
 where
   f xs = safeHead [ ys | i <- [0 .. length xs-1], ys <- applyAll (ruleSubVarIndex i) xs ]

-- split rule in two?
ruleSubVarIndex :: Fractional a => Int -> Transformation (LinearSystem a)
ruleSubVarIndex i = makeTrans $ \xs ->
   let (before, (lhs :==: rhs):after) = splitAt i xs
   in case S.toList (getVars lhs) of
         [v] | getConstant lhs == 0 && (v `S.member` getVars (before, after) || con/=1) ->
            Just $ subst before ++ [new] ++ subst after
          where 
            con    = coefficientOf v lhs
            newrhs = toLinearExpr (1/con) * rhs
            new    = var v :==: newrhs
            subst  = substEquations (singletonSubst v newrhs)
         _ -> Nothing

ruleFreeVarsToRight :: Num a => Rule (LinearSystem a)
ruleFreeVarsToRight = makeSimpleRule "FreeVarsToRight" f
 where
   f xs = 
      let vars = [ head ys | ys <- map (S.toList . getVars . getLHS) xs, not (null ys) ]
          change eq =
             let (e1, e2) = splitLinearExpr (`notElem` vars) (getLHS eq) -- constant ends up in e1
             in e2 :==: getRHS eq - e1
      in Just (map change xs)

------------------------------------------------------------------
-- toEchelon strategy

toGeneralSolution :: Fractional a => Strategy (EqsInContext a)
toGeneralSolution = bringToStandardForm <*> toEchelon <*> determineFreeVars <*> echelonToSolution
 where
   bringToStandardForm = repeatS ruleToStandardForm
   toEchelon           = repeatS (Strategy.check (not . null . remaining) <*> try ruleExchange <*> repeatS ruleEliminateVar <*> ruleCoverTop)
   determineFreeVars   = liftSystemRule ruleFreeVarsToRight
   echelonToSolution   = repeatS (liftSystemRule ruleSubVar <*> liftSystemRule ruleFreeVarsToRight)

ruleToStandardForm :: Num a => Rule (EqsInContext a)
ruleToStandardForm = liftSystemRule $ liftRuleToList $ makeRule "ToStandardForm" equationToStandardForm 

ruleEliminateVar :: Fractional a => Rule (EqsInContext a)
ruleEliminateVar = makeRule "Eliminate Variable" $ app3 (\x y z -> liftSystemTrans $ addEquations x y z) args
 where
   args c = do 
      mv <- minvar c
      let hd:rest = remaining c
          getCoef = coefficientOf mv . getLHS
      (i, coef) <- safeHead [ (i, c) | (i, eq) <- zip [0..] rest, let c = getCoef eq, c /= 0 ]
      let v = negate coef / getCoef hd
      return (i + covered c + 1, covered c, v)

ruleCoverTop :: Fractional a => Rule (EqsInContext a)
ruleCoverTop = minorRule $ makeSimpleRule "CoverTop" $ \c ->
   return c {covered = covered c + 1}

-- Context for toEchelon strategy

data EqsInContext a = EIC
   { equations  ::  LinearSystem a
   , covered    ::  Int
   }
 deriving Show

-- | The equations that remain to be solved
remaining :: EqsInContext a -> Equations (LinearExpr a)
remaining c = drop (covered c) (equations c)

-- | The minimal variable in the remaining equations
minvar :: EqsInContext a -> Maybe String
minvar c | S.null set = Nothing
         | otherwise  = Just (S.findMin set)
 where
   set = getVars (remaining c) 
   
inContext :: LinearSystem a -> EqsInContext a
inContext e = EIC e 0

instance Eq a => Eq (EqsInContext a) where
  x==y = equations x == equations y

ruleExchange :: Rule (EqsInContext a)
ruleExchange = makeRule "Exchange" $ app2 (\x y -> liftSystemTrans $ exchange x y) args
 where
   args c = do
       let eqs = remaining c
       mv <- minvar c
       i  <- findIndex (S.member mv . getVars) eqs
       return (covered c, covered c + i)
       
selectArgs :: (EqsInContext a -> Rule (LinearSystem a)) -> EqsInContext a -> Maybe (EqsInContext a)
selectArgs f c = do
   new <- apply (f c) (equations c)
   return c {equations = new}

liftRuleToList :: Rule a -> Rule [a]
liftRuleToList r = makeSimpleRule (name r) f 
 where
   f xs = let rs = map (`ruleOnIndex` r) [0 .. length xs-1]
          in apply (combineRules rs) xs

ruleOnIndex :: Int -> Rule a -> Rule [a]
ruleOnIndex i = liftRule (LiftPair get set) 
 where
   get     = safeHead . drop i 
   set new = zipWith (\j -> if i==j then const new else id) [0..]

transOnIndex :: Int -> Transformation a -> Transformation [a]
transOnIndex i f = makeTrans $ \as -> 
   case splitAt i as of
      (xs, y:ys) -> do 
         new <- apply f y 
         return $ xs ++ [new] ++ ys 
      _          -> Nothing
   
liftSystemRule :: Rule (LinearSystem a) -> Rule (EqsInContext a)
liftSystemRule = liftRule $ LiftPair get set
 where
   get = return . equations
   set new c = c {equations = new}

liftSystemTrans :: Transformation (LinearSystem a) -> Transformation (EqsInContext a)
liftSystemTrans f = makeTrans $ \c -> do
   new <- apply f (equations c) 
   return c {equations = new}

liftLeft :: Strategy a -> Strategy (Either a b)
liftLeft = mapStrategy $ liftRule $ 
   LiftPair (either Just (const Nothing)) (\a -> either (const $ Left a) Right)

liftRight :: Strategy b -> Strategy (Either a b)
liftRight = mapStrategy $ liftRule $ 
   LiftPair (either (const Nothing) Just) (\a -> either Left (const $ Right a))

translation :: String -> (a -> Maybe b) -> Rule (Either a b)
translation s f = makeSimpleRule s (either (fmap Right . f) (const Nothing))

equationsToMatrix :: Fractional a => Strategy (Either (LinearSystem a)(MatrixInContext a))
equationsToMatrix = translation "SystemToMatrix" (Just . Matrix.inContext . systemToMatrix) <*> liftRight toReducedEchelon

-----------------------------------------------------
-- 9.1 Oplossen van lineaire vergelijkingen

default (Rational)

x1 = var "x1"
x2 = var "x2"
x3 = var "x3"
x4 = var "x4"
x5 = var "x5"
z1 = var "z1"
z2 = var "z2"
z3 = var "z3"

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
   
ex3a = 
   [ z1    + i*z2 + z3       :==: 1
   ,           z2 + (i+1)*z3 :==: 0
   , -i*z1 +   z2            :==: 0
   ]
   
ex3b lam = 
   [ (1 - lam)*z1 - 2*z2 :==: 0
   , 5*z1 + (3- lam) *z2 :==: 0
   ]

ex3b1 = ex3b (2+3*i)
ex3b2 = ex3b (2-3*i)

ex3c :: LinearExpr SimpleNum -> LinearSystem SimpleNum
ex3c lam = 
   [ lam*z1 - z2 :==: 0
   , lam*z2 + z3 :==: 0
   , z1 + lam*z3 :==: 0
   ]

ex3c1 = ex3c 1
ex3c2 = ex3c (e ) -- #^ (2 * pi_ / 3))
ex3c3 = ex3c (e ) -- #^ (-2 * pi_ / 3))

ex4 lam = 
   [ x1 - 2 * x3 :==: lam + 4
   , -2 * x1 + lam * x2 + 7 * x3 :==: -14
   , -1 * x1 + lam * x2 + 6*x3 :==: lam - 12
   ]

ex5 lam = 
   [ x1 + 2*x2 + 3*x3 - 6*x4 :==: -1
   , 2*x1 + x2 + 9*x4 :==: 2 + lam
   , -3*x1 - 3*x2 - 3*x3 - 3*x4 :==: 1 + lam
   ] 

i, e, pi_ :: LinearExpr SimpleNum
i = undefined
e = toLinearExpr $ SN (Var "e")
pi_ = undefined
(#^) = undefined
lamvar = toLinearExpr $ SN (Var "Lambda")

testje  = traceStrategy toGeneralSolution (inContext ex1a') -- '
testje2 = traceStrategy toGeneralSolution (inContext ex3c2)
testje3 = traceStrategy toGeneralSolution (inContext $ ex4 lamvar)
testje4 = traceStrategy toGeneralSolution (inContext $ ex5 lamvar)

ex1a' :: LinearSystem SimpleNum
ex1a' = 
   [   x1 + 2*x2 + 3*x3 - x4   :==:  0 
   , 2*x1 + 3*x2 - x3 + 3*x4   :==:  0
   , 4*x4 + 6*x2 + x3 + 2*x4   :==:  0
   ]

-- in normal form
newtype SimpleNum = SN MyNum deriving (Show, Eq, Ord)

instance Num SimpleNum where
   SN a + SN b = SN $ nf $ a + b
   SN a * SN b = SN $ nf $ a * b
   n - m = n + negate m
   negate (SN a) = SN $ nf $ Neg a
   fromInteger = SN . nf . fromInteger
   
instance Fractional SimpleNum where
   SN a / SN b = SN $ nf $ a / b
   fromRational = SN . nf . fromRational
   recip n = 1 / n 
   
nf :: MyNum -> MyNum
nf x = let (x1, x2) = numFraction x in
       case (simpl x1, simpl x2) of 
          (Con a, Con b) -> Con (a/b)
          (Con 0, _)     -> Con 0
          (a, Con 1)     -> a
          (a, b)         -> a / b
   
{- (/), fromRational, (+), (*), negate -}
{- EXTRA: constants (i,e,pi), powera -}
data MyNum = Con Rational | Var String | MyNum :+ MyNum | MyNum :* MyNum | MyNum :/ MyNum | Neg MyNum
 deriving (Show, Eq, Ord)

infixl 7 :*, :/
infixl 6 :+

infix 1 ~=
x ~= y = let (a, b) = numFraction x
             (c, d) = numFraction y 
         in simpl (a * d) == simpl (b * c)

instance Fractional MyNum where
   (/) = (:/)
   fromRational = Con
   recip n = 1 / n

instance Num MyNum where
   (+) = (:+)
   (*) = (:*)
   n - m = n + negate m
   negate = Neg
   fromInteger = fromRational . fromInteger
   
   -- Not supported by this instance
   abs    = error "Not supported: abs"   
   signum = error "Not supported: signum"

{- sumNum, productNum :: MyNum -> [MyNum]
sumNum (a :+ b) = sumNum a ++ sumNum b
sumNum x = [x]
productNum (a :* b) = productNum a ++ productNum b
productNum x = [x] -}

simpl :: MyNum -> MyNum
simpl x = 
   let vs = numVars x
       v  = minimum vs
       (a, b) = numSplit v x
       con = simpl (simpl2 b)
       var = simpl2 (Var v :* simpl (simpl2 a))
   in if null vs then simpl2 x else 
      case con of 
         Con 0 -> var
         _     -> var :+ con

simpl2 :: MyNum -> MyNum
simpl2 = simpl4 . simpl3

simpl3 :: MyNum -> MyNum -- push Negs inside
simpl3 this = 
   case this of
      Neg a -> case a of
                  Var _  -> this
                  Con x  -> Con (negate x)
                  b :+ c -> simpl3 (Neg b) :+ simpl3 (Neg c)
                  b :* c -> simpl3 (Neg b) :* simpl3 c
                  b :/ c -> simpl3 (Neg b) :/ simpl3 c
                  Neg b -> simpl3 b
      a :+ b -> simpl3 a :+ simpl3 b
      a :* b -> simpl3 a :* simpl3 b
      a :/ b -> simpl3 a :/ simpl3 b
      _ -> this
      
simpl4 :: MyNum -> MyNum
simpl4 this = 
   case this of
      a :+ b -> case (simpl4 a, simpl4 b) of
                   (Con x, Con y) -> Con (x+y)
                   (Con 0, c) -> c
                   (c, Con 0) -> c
                   (c :+ d, e) -> c :+ (d :+ e)
                   (c, d) -> c :+ d
      a :* b -> case (simpl4 a, simpl4 b) of
                   (Con x, Con y) -> Con (x*y)
                   (Con 0, c) -> Con 0
                   (c, Con 0) -> Con 0
                   (Con 1, c) -> c
                   (c, Con 1) -> c
                   (c :* d, e) -> c :* (d :* e)
                   (c, d) -> c :* d
      a :/ b -> case (simpl4 a, simpl4 b) of
                   (Con x, Con y) -> Con (x/y)
                   (Con 0, c) -> Con 0
                   (c, Con 1) -> c
                   (c, d) -> c :/ d
      _ -> this
        
numVars :: MyNum -> [String]
numVars this = 
   case this of
      Var y  -> [y]
      Con x  -> []
      a :+ b -> numVars a ++ numVars b
      a :* b -> numVars a ++ numVars b
      Neg a  -> numVars a
      a :/ b -> numVars a ++ numVars b
      
numFraction :: MyNum -> (MyNum, MyNum)
numFraction this =
   case this of
      Var _  -> (this, Con 1)
      Con _  -> (this, Con 1)
      a :+ b -> let (a1, a2) = numFraction a
                    (b1, b2) = numFraction b
                in ((a1:*b2) :+ (b1:*a2), a2 :* b2)
      a :* b -> let (a1, a2) = numFraction a
                    (b1, b2) = numFraction b
                in (a1:*b1, a2:*b2)
      Neg a  ->  let (a1, a2) = numFraction a
                in (Neg a1, a2)
      a :/ b -> let (a1, a2) = numFraction a
                    (b1, b2) = numFraction b
                in (a1:*b2, a2:*b1)

numSplit :: String -> MyNum -> (MyNum, MyNum)
numSplit x this =
   case this of
      Var y | x==y -> (Con 1, Con 0)
      a :+ b -> let (a1, a2) = numSplit x a
                    (b1, b2) = numSplit x b
                in (a1 :+ b1, a2 :+ b2)
      a :* b -> let (a1, a2) = numSplit x a
                    (b1, b2) = numSplit x b
                in ((a1 :* b) :+ (a2 :* b1), a2 :* b2)
      Neg a  -> let (a1, a2) = numSplit x a
                in (Neg a1, Neg a2)
      _ :/ _ -> error "(:/) in numSplit"
      _ -> (Con 0, this)

fracSound :: (String -> Rational) -> MyNum -> Bool
fracSound f n = lhs == rhs 
 where (x, y) = numFraction n
       lhs = evalNum f n
       rhs = evalNum f x / evalNum f y
   
splitSound :: (String -> Rational) -> MyNum -> Bool
splitSound f n = evalNum f n == evalNum f ((x :* Var "x") :+ y)
 where (x, y) = numSplit "x" n

simplSound :: (String -> Rational) -> MyNum -> Bool
simplSound f n = evalNum f x == evalNum f (simpl x)
 where (x, y) = numFraction n

simplSound2 :: (String -> Rational) -> MyNum -> Bool
simplSound2 f n = evalNum f x == evalNum f (simpl2 x)
 where (x, y) = numFraction n
 
evalNum :: (String -> Rational) -> MyNum -> Rational
evalNum f this = 
   case this of
      Var y  -> f y
      Con x  -> x
      a :+ b -> evalNum f a + evalNum f b
      a :* b -> evalNum f a * evalNum f b
      Neg a  -> negate (evalNum f a)
      a :/ b -> evalNum f a / evalNum f b
   
sizeNum :: MyNum -> Int
sizeNum this = 
   case this of
      Var y  -> 1
      Con x  -> 1
      a :+ b -> 1 + sizeNum a + sizeNum b
      a :* b -> 1 + sizeNum a + sizeNum b
      Neg a  -> 1 + sizeNum a
      a :/ b -> 1 + sizeNum a + sizeNum  b
      
instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
   arbitrary = liftM2 (/) arb arb
    where arb = do x <- arbInt ; return (fromIntegral x)

arbInt :: Gen Int
arbInt = do x <- arbitrary; return (abs x + 1)

isZero, notZero :: MyNum -> Bool
notZero = not . isZero
isZero (Con n)  = n==0
isZero (Var _)  = False
isZero (n :+ m) = n ~= negate m
isZero (n :* m) = isZero n || isZero m
isZero (n :/ m) = isZero n
isZero (Neg n)  = isZero n
{-
somewh :: (MyNum -> Maybe MyNum) -> MyNum -> Maybe MyNum
somewh f this = f this `mplus` 
   case this of
      a :+ b -> fmap (:+ b) (somewh f a) `mplus` fmap (a :+) (somewh f b)
      a :* b -> fmap (:* b) (somewh f a) `mplus` fmap (a :*) (somewh f b)
      a :/ b -> fmap (:/ b) (somewh f a) `mplus` fmap (a :/) (somewh f b)
      Neg a  -> fmap Neg (somewh f a)
      _      -> Nothing -}
{-
numS :: Strategy MyNum
numS =  exhaustive (map liftNumRule $ negRules ++ simplerRules)
    <*> repeatS (liftNumRule r16) -- distribute plus over times
    <*> order
    
         -- [r1,r2,r3,r4,r9,r10,r11,r12] -- simplification
         -- ++ [r5,r6,r7,r8] -- (linear) rewrite
         -- ++ [r16,r17,r18] -- non-linear distribution 
         
 where
   simplerRules = [r1,r2,r3]
   negRules = [r4,r11,r17,r18] -- push negations inward
   
   order = makeSimpleRule "Order" $ \x -> return $ 
      foldr1 (:+) $ sort (map (\y -> foldr1 (:*) $ sort $ productNum y) $ sumNum x)
   
   r1 = makeSimpleRule "PlusUnit" f where
           f (Con 0 :+ x) = return x
           f (x :+ Con 0) = return x
           f _            = Nothing
   r2 = makeSimpleRule "TimesZero" f where
           f (Con 0 :* x) = return $ Con 0
           f (x :* Con 0) = return $ Con 0
           f _            = Nothing
   r3 = makeSimpleRule "TimesUnit" f where
           f (Con 1 :* x) = return x
           f (x :* Con 1) = return x
           f _            = Nothing
   r4 = makeSimpleRule "ConstantPropagation" f where
           f (Con x :+ Con y) = return $ Con (x+y)
           f (Con x :* Con y) = return $ Con (x*y)
           f (Con x :/ Con y) = return $ Con (x/y)
           f (Neg (Con x))    = return $ Con (negate x)
           f _                = Nothing 
   r5 = makeSimpleRule "TransPlus" f where
           f ((a :+ b) :+ c) = return $ a :+ (b :+ c)
           f _               = Nothing
   r6 = makeSimpleRule "TransTimes" f where
           f ((a :* b) :* c) = return $ a :* (b :* c)
           f _               = Nothing
   r7 = makeSimpleRule "CommPlus" f where          
           f (x :+ (y :+ z)) | y<x = return (y :+ (x :+ z)) 
                             | otherwise = Nothing
           f (x :+ y)        | y<x = return (y :+ x)
           f _                     = Nothing
   r8 = makeSimpleRule "CommTimes" f where
           f (x :* (y :* z)) | y<x = return (y :* (x :* z))
                             | otherwise = Nothing
           f (x :* y)        | y<x = return (y :* x)
           
           f _                     = Nothing
   r9 = makeSimpleRule "ConPlus" f where
           f (Con x :+ (Con y :+ z)) = return $ Con (x+y) :+ z
           f _                       = Nothing            
   r10 = makeSimpleRule "ConTimes" f where
           f (Con x :* (Con y :* z)) = return $ Con (x*y) :* z
           f _                       = Nothing 
   r11 = makeSimpleRule "NegNeg" f where
           f (Neg (Neg x)) = return x
           f _             = Nothing    
   r12 = makeSimpleRule "Div" f where
           f (x :/ y) | x==y = return (Con 1)
           f _               = Nothing       
   r13 = makeSimpleRule "SameDiv" f where
           f ((x :/ z1) :+ (y :/ z2))        | z1==z2 = return $ (x:+y) :/ z1
           f ((x :/ z1) :+ ((y :/ z2) :+ u)) | z1==z2 = return $ ((x:+y) :/ z1) :+ u
           f _                                        = Nothing   
   r14 = makeSimpleRule "DivOne" f where
           f (x :/ Con 1) = return x
           f _            = Nothing 
   r15 = makeSimpleRule "ZeroDiv" f where
           f (Con 0 :/ x) = return $ Con 0
           f _            = Nothing 
   r16 = makeSimpleRule "TimesOverPlus" f where
           f (x :* (y :+ z)) = return $ (x :* y) :+ (x :* z) 
           f ((x :+ y) :* z) = return $ (x :* z) :+ (y :* z) 
           f _               = Nothing
   r17 = makeSimpleRule "NegPlus" f where
           f (Neg (x :+ y)) = return $ Neg x :+ Neg y
           f _              = Nothing   
   r18 = makeSimpleRule "NegTimes" f where
           f (Neg (x :* y)) = return $ Neg x :* y
           f _              = Nothing   
                                                                                      
liftNumRule :: Rule MyNum -> Rule MyNum
liftNumRule r = makeSimpleRule (name r) (somewh $ apply r)

simplify :: MyNum -> MyNum
simplify = head . runStrategy numS -}

instance Arbitrary MyNum where
   arbitrary = sized (\n -> arbNum n)
   coarbitrary (Con (n :% m)) = variant 0 . coarbitrary n . coarbitrary m
   coarbitrary (n :+ m)       = variant 1 . coarbitrary n . coarbitrary m
   coarbitrary (n :* m)       = variant 2 . coarbitrary n . coarbitrary m
   coarbitrary (n :/ m)       = variant 3 . coarbitrary n . coarbitrary m
   coarbitrary (Neg n)        = variant 4 . coarbitrary n

arbNum :: Int -> Gen MyNum
arbNum 0 = oneof [liftM fromInteger arbitrary, return $ Var "x"]
arbNum n = oneof [arbNum 0, liftM2 (:+) rec rec, liftM2 (:*) rec rec, liftM2 (:/) rec recNZ, liftM Neg rec]
 where 
   rec   = arbNum   (n `div` 2)
   recNZ = arbNumNZ (n `div` 2)

-- non-zero value
arbNumNZ :: Int -> Gen MyNum
arbNumNZ 0 = oneof [liftM fromIntegral arbIntNZ, return $ Var "x"]
arbNumNZ n = oneof [arbNumNZ 0, liftM2 (:*) recNZ recNZ, liftM2 (:/) recNZ recNZ, liftM Neg recNZ]
 where
   rec   = arbNum   (n `div` 2)
   recNZ = arbNumNZ (n `div` 2)
     
arbIntNZ :: Gen Int
arbIntNZ = do
   n <- arbitrary
   if n==0 then arbIntNZ else return n     
      
checks :: IO ()
checks = do
   putStrLn "[1]"
   quickCheck $ communtative (+)
   quickCheck $ associative (+)
   quickCheck $ unit (+) 0
   quickCheck $ communtative (*)
   quickCheck $ associative (*)
   quickCheck $ unit (*) 1
   quickCheck $ zero (*) 0
   quickCheck $ rightUnit (/) 1
   quickCheck $ \x -> notZero x ==> leftZero (/) 0 x

   putStrLn "[2]"
   quickCheck $ \x -> negate (negate x) ~= x
   quickCheck $ \x -> (notZero x) ==> x/x ~= 1

   --distribution rules
   putStrLn "[3]"
   quickCheck $ \x y   -> negate x + y ~= y - x
   quickCheck $ \x y   -> x + negate y ~= x - y
   quickCheck $ \x y z -> (notZero z) ==> (x/z) + (y/z) ~= (x+y)/z 
   quickCheck $ \x y z -> (x+y) * z ~= (x*z) + (y*z)
   quickCheck $ \x y z -> x * (y+z) ~= (x*y) + (x*z)
   quickCheck $ \x y   -> (negate x) * y ~= negate (x*y)
   quickCheck $ \x y   -> x * (negate y) ~= negate (x*y)
   quickCheck $ \x y   -> negate (x+y) ~= negate x + negate y

   quickCheck $ \x y z -> (notZero y) ==> (x/y) * z ~= (x*z)/y
   quickCheck $ \x y z -> (notZero z) ==> x * (y/z) ~= (x*y)/z
   quickCheck $ \x y   -> (notZero y) ==> (negate x)/y ~= negate (x/y) 
   quickCheck $ \x y   -> (notZero y) ==> x/(negate y) ~= negate (x/y) 
   quickCheck $ \x y z -> (notZero y && notZero z) ==> x/(y*z) ~= (x/y)/z
   quickCheck $ \x y z -> (notZero y && notZero z) ==> x/(y/z) ~= (x*z) / y
   quickCheck $ \x y   -> (notZero y) ==> x/y ~= x * (1/y)
   quickCheck $ \x y z u -> (notZero y && notZero u) ==> (x/y) * (z/u) ~= (x*z) / (y*u)

communtative op x y  = op x y ~= op y x
associative op x y z = op x (op y z) ~= op (op x y) z 
leftUnit op e x      = op e x ~= x
rightUnit op e x     = op x e ~= x
unit op e x          = leftUnit op e x && rightUnit op e x
leftZero op e x      = op e x ~= 0
rightZero op e x     = op x e ~= 0
zero op e x          = leftZero op e x && rightZero op e x