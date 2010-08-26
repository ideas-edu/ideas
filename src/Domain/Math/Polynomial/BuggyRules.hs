-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Some buggy rules catching common misconceptions (also on the abc-formula)
--
-----------------------------------------------------------------------------
module Domain.Math.Polynomial.BuggyRules where

import Prelude hiding ((^))
import Common.Id
import Common.Rewriting
import Domain.Math.Expr
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Numeric.Views
import Domain.Math.Data.Polynomial
import Domain.Math.Equation.CoverUpRules
import Common.Classes
import Common.Context
import Common.View
import Common.Transformation (Rule, buggyRule, siblingOf, Transformation, useRecognizer, supply1, makeTransList)
import Control.Monad
import qualified Common.Transformation as Rule

makeRule           = buggyName Rule.makeRule
makeSimpleRule     = buggyName Rule.makeSimpleRule
makeSimpleRuleList = buggyName Rule.makeSimpleRuleList
ruleList           = buggyName Rule.ruleList

buggyName f s = f ("algebra.equations.buggy" # s)

buggyRulesExpr :: [Rule Expr]
buggyRulesExpr = 
   map (siblingOf distributeTimes)
   [ buggyDistrTimes, buggyDistrTimesForget, buggyDistrTimesSign
   , buggyDistrTimesTooMany, buggyDistrTimesDenom
   ] ++
   [ buggyMinusMinus, buggyPriorityTimes -- no sibling defined
   ]

buggyRulesEquation :: [Rule (Equation Expr)]
buggyRulesEquation = 
   [ buggyPlus, buggyNegateOneSide, siblingOf flipEquation buggyFlipNegateOneSide
   , buggyNegateAll
   , buggyDivNegate, buggyDivNumDenom, buggyCancelMinus
   , buggyMultiplyOneSide, buggyMultiplyForgetOne
   ]

buggyPlus :: Rule (Equation Expr)
buggyPlus = describe "Moving a term from the left-hand side to the \
   \right-hand side (or the other way around), but forgetting to change \
   \the sign." $ 
   buggyRule $ makeSimpleRuleList "plus" $ \(lhs :==: rhs) -> do
      (a, b) <- matchM plusView lhs
      [ a :==: rhs + b, b :==: rhs + a ]
    `mplus` do
      (a, b) <- matchM plusView rhs
      [ lhs + a :==: b, lhs + b :==: a ]

buggyNegateOneSide :: Rule (Equation Expr)
buggyNegateOneSide = describe "Negate terms on one side only." $
   buggyRule $ makeSimpleRuleList "negate-one-side" $ \(lhs :==: rhs) -> do
      [ -lhs :==: rhs, lhs :==: -rhs  ] 

buggyFlipNegateOneSide :: Rule (Equation Expr)
buggyFlipNegateOneSide = describe "Negate terms on one side only." $
   buggyRule $ makeSimpleRuleList "flip-negate-one-side" $ \(lhs :==: rhs) -> do
      [ -rhs :==: lhs, rhs :==: -lhs  ]

buggyNegateAll :: Rule (Equation Expr)
buggyNegateAll = describe "Negating all terms (on both sides of the equation, \
   \but forgetting one term." $
   buggyRule $ makeSimpleRuleList "negate-all" $ \(lhs :==: rhs) -> do 
      xs <- matchM sumView lhs
      ys <- matchM sumView rhs
      let makeL i = makeEq (zipWith (f i) [0..] xs) (map negate ys)
          makeR i = makeEq (map negate xs) (zipWith (f i) [0..] ys)
          makeEq as bs = build sumView as :==: build sumView bs
          f i j = if i==j then id else negate
          len as = let n = length as in if n < 2 then -1 else n
      map makeL [0 .. len xs] ++ map makeR [0 .. len ys]

buggyDivNegate :: Rule (Equation Expr)
buggyDivNegate = describe "Dividing, but wrong sign." $
   buggyRule $ makeSimpleRuleList "divide-negate" $ \(lhs :==: rhs) -> do
      (a, b) <- matchM timesView lhs
      [ b :==: rhs/(-a) | noVars a ] ++ [ a :==: rhs/(-b) | noVars b ]
    `mplus` do
      (a, b) <- matchM timesView rhs
      [ lhs/(-a) :==: b | noVars a ] ++ [ lhs/(-b) :==: a | noVars b ]

buggyDivNumDenom :: Rule (Equation Expr)
buggyDivNumDenom = describe "Dividing both sides, but swapping \
   \numerator/denominator." $
   buggyRule $ makeSimpleRuleList "divide-numdenom" $ \(lhs :==: rhs) -> do
      (a, b) <- matchM timesView lhs
      [ b :==: a/rhs | noVars rhs ] ++ [ a :==: b/rhs | noVars rhs ]
    `mplus` do
      (a, b) <- matchM timesView rhs
      [ a/lhs :==: b | noVars lhs ] ++ [ b/lhs :==: a | noVars lhs ]

buggyDistrTimes :: Rule Expr
buggyDistrTimes = describe "Incorrect distribution of times over plus: one \
   \term is not multiplied." $
   buggyRule $ makeSimpleRuleList "distr-times-plus" $ \expr -> do
      (a, (b, c)) <- matchM (timesView >>> second plusView) expr
      [ a*b+c, b+a*c ]
    `mplus` do
      ((a, b), c) <- matchM (timesView >>> first plusView) expr
      [ a*c+b, a+b*c ]

buggyDistrTimesForget :: Rule Expr
buggyDistrTimesForget = describe "Incorrect distribution of times over plus: \
   \one term is forgotten." $
   buggyRule $ makeSimpleRuleList "distr-times-plus-forget" $ \expr -> do
      (a, (b, c)) <- matchM (timesView >>> second plusView) expr
      [ a*bn+a*c | bn <- forget b ] ++ [ a*b+a*cn | cn <- forget c ]
    `mplus` do
      ((a, b), c) <- matchM (timesView >>> first plusView) expr
      [ an*c+b*c | an <- forget a] ++ [ a*c+bn*c | bn <- forget b]
 where
   forget :: Expr -> [Expr]
   forget expr =
      case match productView expr of
         Just (b, xs) | n > 1 -> 
            [ build productView (b, make i) | i <- [0..n-1] ]
          where
            make i = [ x | (j, x) <- zip [0..] xs, i/=j ]
            n = length xs
         _ -> [0]

buggyDistrTimesSign :: Rule Expr
buggyDistrTimesSign = describe "Incorrect distribution of times over plus: \
   \changing sign of addition." $
   buggyRule $ makeSimpleRuleList "distr-times-plus-sign" $ \expr -> do
      (a, (b, c)) <- matchM (timesView >>> second plusView) expr
      [ a.*.b .-. a.*.c ]
    `mplus` do
      ((a, b), c) <- matchM (timesView >>> first plusView) expr
      [ a.*.c .-. b.*.c ]

buggyDistrTimesTooMany :: Rule Expr
buggyDistrTimesTooMany = describe "Strange distribution of times over plus: \
   \a*(b+c)+d, where 'a' is also multiplied to d." $ 
   buggyRule $ makeSimpleRuleList "distr-times-too-many" $ \expr -> do
      ((a, (b, c)), d) <- matchM (plusView >>> first (timesView >>> second plusView)) expr
      [cleanUpExpr2 $ a*b+a*c+a*d]

buggyDistrTimesDenom :: Rule Expr
buggyDistrTimesDenom = describe "Incorrct distribution of times over plus: \
   \one of the terms is a fraction, and the outer expression is multiplied by \
   \the fraction's denominator." $
   buggyRule $ makeSimpleRuleList "distr-times-denom" $ \expr -> do
      (a, (b, c)) <- matchM (timesView >>> second plusView) expr
      [(1/a)*b + a*c, a*b + (1/a)*c]
    `mplus` do
      ((a, b), c) <- matchM (timesView >>> first plusView) expr
      [a*(1/c) + b*c, a*c + b*(1/c)]

buggyMinusMinus :: Rule Expr
buggyMinusMinus = describe "Incorrect rewriting of a-(b-c): forgetting to \
   \change sign." $
   buggyRule $ makeSimpleRule "minus-minus" $ \expr ->
      case expr of
         a :-: (b :-: c)  -> Just (a-b-c)
         Negate (a :-: b) -> Just (a-b) 
         _ -> Nothing

buggyCancelMinus :: Rule (Equation Expr)
buggyCancelMinus = describe "Cancel terms on both sides, but terms have \
   \different signs." $
   buggyRule $ makeSimpleRuleList "cancel-minus" $ \(lhs :==: rhs) -> do
      xs <- matchM sumView lhs
      ys <- matchM sumView rhs  
      [ eq | (i, x) <- zip [0..] xs, (j, y) <- zip [0..] ys
           , cleanUpExpr2 x == cleanUpExpr2 (-y) 
           , let f n as = build sumView $ take n as ++ drop (n+1) as
           , let eq = f i xs :==: f j ys
           ]

buggyPriorityTimes :: Rule Expr
buggyPriorityTimes = describe "Prioity of operators is changed, possibly by \
   \ignoring some parentheses." $
   buggyRule $ makeSimpleRuleList "priority-times" $ \expr -> do
      (a, (b, c)) <- matchM (plusView >>> second timesView) expr
      [(a+b)*c]
    `mplus` do
      ((a, b), c) <- matchM (plusView >>> first timesView) expr
      [a*(b+c)]

buggyMultiplyOneSide :: Rule (Equation Expr)
buggyMultiplyOneSide = describe "Multiplication on one side of the equation only" $
   buggyRule $ makeRule "multiply-one-side" $ 
   useRecognizer recognizeEq $ supply1 (const (Just 2)) multiplyOneSide
 where
   recognizeEq eq1@(a1 :==: a2) eq2@(b1 :==: b2) =
      let p r  = r `notElem` [-1, 0, 1] && 
                 any (myEq eq2) (applyAll (multiplyOneSide r) eq1)
      in maybe False p (recognizeMultiplication a1 b1) 
      || maybe False p (recognizeMultiplication a2 b2)

recognizeMultiplication :: Expr -> Expr -> Maybe Rational
recognizeMultiplication a b = do
   (_, pa) <- match (polyViewWith rationalView) a 
   (_, pb) <- match (polyViewWith rationalView) b
   let d = coefficient (degree pa) pa
   guard (d /= 0)
   return (coefficient (degree pb) pb / d)
   
multiplyOneSide :: Rational -> Transformation (Equation Expr)
multiplyOneSide r = makeTransList $ \(lhs :==: rhs) -> do
      xs <- matchM sumView lhs
      ys <- matchM sumView rhs
      let f = map (*fromRational r)
      [build sumView (f xs) :==: rhs, lhs :==: build sumView (f ys)]

buggyMultiplyForgetOne :: Rule (Equation Expr)
buggyMultiplyForgetOne = describe "Multiply the terms on both sides of the \
   \equation, but forget one." $
   buggyRule $ makeRule "multiply-forget-one" $ 
   useRecognizer recognizeEq $ supply1 (const (Just 2)) multiplyForgetOne
 where
   recognizeEq eq1@(a1 :==: a2) eq2@(b1 :==: b2) =
      let p r  = r `notElem` [-1, 0, 1] && 
                 any (myEq eq2) (applyAll (multiplyForgetOne r) eq1)
      in maybe False p (recognizeMultiplication a1 b1) 
      || maybe False p (recognizeMultiplication a2 b2)

multiplyForgetOne :: Rational -> Transformation (Equation Expr)
multiplyForgetOne r = makeTransList $ \(lhs :==: rhs) -> do
   xs <- matchM sumView lhs
   ys <- matchM sumView rhs
   let makeL i = f (zipWith (mul . (/=i)) [0..] xs) (map (mul True) ys)
       makeR i = f (map (mul True) xs) (zipWith (mul . (/=i)) [0..] ys) 
       f as bs = build sumView as :==: build sumView bs
       mul b   = if b then (*fromRational r) else id
   do guard (length xs > 1) 
      map makeL [0 .. length xs-1]
    `mplus` do
      guard (length ys > 1)
      map makeR [0 .. length ys-1]

-- Redundant function; should come from exercise
myEq :: Equation Expr -> Equation Expr -> Bool
myEq = let eqR f x y = fmap f x == fmap f y in eqR (acExpr . cleanUpExpr2)

---------------------------------------------------------
-- Quadratic and Higher-Degree Polynomials

buggyQuadratic :: IsTerm a => [Rule (Context a)]
buggyQuadratic =
   map use
      [ buggyCoverUpTimesMul, buggyCoverUpEvenPower
      , buggyCoverUpTimesWithPlus, buggyDivisionByVarBoth
      , buggyDivisionByVarZero
      ] ++
   map use
      [ buggyDistributionSquare, buggyDistributionSquareForget
      , buggySquareMultiplication
      ] ++
   map use
      [ buggyCoverUpEvenPowerTooEarly, buggyCoverUpEvenPowerForget
      , buggyCoverUpSquareMinus
      ]

buggyCoverUpEvenPower :: Rule (Equation Expr)
buggyCoverUpEvenPower = describe "Covering up an even power, but forgetting \
   \the negative root" $ buggyRule $ siblingOf coverUpPower $
   makeSimpleRuleList "coverup.even-power" $ \(lhs :==: rhs) ->
      make (:==:) lhs rhs ++ make (flip (:==:)) rhs lhs
 where
   make equals ab c = do 
      (a, b) <- isBinary powerSymbol ab
      n <- matchM integerView b
      guard (n > 0 && even n)
      return (a `equals` root c (fromInteger n))

buggyCoverUpEvenPowerTooEarly :: Rule (OrList (Equation Expr))
buggyCoverUpEvenPowerTooEarly = describe "Trying to cover up an even power, \
   \but there is some other operation to be done first. Example: x^2+1=9" $
   buggyRule $ siblingOf coverUpPower $ 
   makeSimpleRuleList "coverup.even-power-too-early" $ 
      oneDisjunct $ helperBuggyCUPower True

buggyCoverUpEvenPowerForget :: Rule (OrList (Equation Expr))
buggyCoverUpEvenPowerForget = describe "Trying to cover up an even power, \
   \but there is some other operation to be done first. Example: 9*x^2=81, \
   \ and rewriting this into x=9 or x=-9." $
   buggyRule $ siblingOf coverUpPower $ 
   makeSimpleRuleList "coverup.even-power-forget" $ 
      oneDisjunct $ helperBuggyCUPower False

helperBuggyCUPower :: Bool -> Equation Expr -> [OrList (Equation Expr)]
helperBuggyCUPower mode (lhs :==: rhs) =
   make (:==:) lhs rhs ++ make (flip (:==:)) rhs lhs
 where
   make equals ab c = do
      (sym, xs) <- getFunction ab
      (i, x)    <- zip [0..] xs
      (a, b)    <- isBinary powerSymbol x
      n         <- matchM integerView b
      guard (n > 0 && even n)
      let opa | mode      = function sym (take i xs ++ [a] ++ drop (i+1) xs)
              | otherwise = a
          rb  = root c (fromInteger n)
      return $ orList [opa `equals` rb, opa `equals` (-rb)]

buggyCoverUpTimesMul :: Rule (Equation Expr)
buggyCoverUpTimesMul = describe "Covering-up a multiplication, but instead of \
   \dividing the right-hand side, multiplication is used." $
   buggyRule $ siblingOf coverUpTimes $ 
   makeSimpleRuleList "coverup.times-mul" $ \(lhs :==: rhs) -> do
      guard (rhs /= 0)
      (a, b) <- isTimes lhs
      [a :==: rhs*b, b :==: rhs*a]
    `mplus` do
      guard (lhs /= 0)
      (a, b) <- isTimes rhs
      [lhs*a :==: b, lhs*b :==: a]

buggyDistributionSquare :: Rule Expr
buggyDistributionSquare = describe "Incorrect removal of parentheses in a squared \
   \addition: forgetting the 2ab term" $ 
   buggyRule $ siblingOf distributionSquare $
   ruleList "distr-square" $ 
      [ \a b -> (a+b)^2 :~> a^2+b^2
      , \a b -> (a-b)^2 :~> a^2-b^2
      , \a b -> (a-b)^2 :~> a^2+b^2
      ]

buggyDistributionSquareForget :: Rule Expr
buggyDistributionSquareForget = describe "Incorrect removal of parentheses in a squared \
   \addition: squaring only one term" $ 
   buggyRule $ siblingOf distributionSquare $
   ruleList "distr-square-forget" $ 
      [ \a b -> (a+b)^2 :~> a^2+b
      , \a b -> (a+b)^2 :~> a+b^2
      , \a b -> (a-b)^2 :~> a^2-b
      , \a b -> (a-b)^2 :~> a-b^2
      ]

buggySquareMultiplication :: Rule Expr
buggySquareMultiplication = describe "Incorrect square of a term that involves \
   \a multiplication." $ buggyRule $
   ruleList "square-multiplication" $
      [ \a b -> (a*b)^2 :~> a*b^2
      , \a b -> (a*b)^2 :~> a^2*b
      , \a b -> a*b^2   :~> (a*b)^2
      , \a b -> a^2*b   :~> (a*b)^2
      ] 

buggyCoverUpSquareMinus :: Rule (OrList (Equation Expr))
buggyCoverUpSquareMinus = describe "A squared term is equal to a negative term \
   \on the right-hand side, resulting in an error in the signs" $
   buggyRule $ makeSimpleRule "coverup.square-minus" $ oneDisjunct $ \eq -> 
      case eq of
         Sym s [a, 2] :==: b | s == powerSymbol -> 
            Just $ orList [a :==: sqrt b, a :==: sqrt (-b)]
         _ -> Nothing

buggyCoverUpTimesWithPlus :: Rule (Equation Expr)
buggyCoverUpTimesWithPlus = describe "Covering-up a multiplication, with an \
   \addition on the other side. Only one of the terms is divided." $ 
   buggyRule $ makeSimpleRuleList "coverup.times-with-plus" $ 
   \(lhs :==: rhs) -> make (:==:) lhs rhs ++ make (flip (:==:)) rhs lhs
 where
   make equals ab cd = do
      (a, b) <- isTimes ab
      (c, d) <- isPlus cd
      [ a `equals` (c/b+d), a `equals` (c+d/b), 
        b `equals` (c/a+d), b `equals` (c+d/a) ]
        
buggyDivisionByVarBoth :: Rule (Equation Expr)
buggyDivisionByVarBoth = describe "Divide both sides by variable, without \
   \introducing the x=0 alternative." $ 
   buggyRule $ makeSimpleRule "division-by-var-both" $ 
   \(lhs :==: rhs) -> do
      (s1, p1) <- match polyView lhs
      (s2, p2) <- match polyView rhs
      let n = lowestDegree p1 `min` lowestDegree p2
      guard (n > 0 && s1==s2)
      let f p = build polyView (s1, raise (-n) p)
      return (f p1 :==: f p2)

buggyDivisionByVarZero :: Rule (Equation Expr)
buggyDivisionByVarZero = describe "Divide both sides by variable, without \
   \introducing the x=0 alternative." $ 
   buggyRule $ makeSimpleRuleList "division-by-var-zero" $ 
   \(lhs :==: rhs) -> do
      guard (rhs == 0)
      (s, p) <- matchM polyView lhs
      let n = lowestDegree p
      guard (n > 0)
      -- Quick fix to do some trivial steps for a linear equation, so that
      -- buggy rules are recognized. 
      let eq = build polyView (s, raise (-n) p) :==: 0
      eq : applyM coverUpPlus eq

---------------------------------------------------------
-- ABC formula misconceptions

abcBuggyRules :: [Rule (OrList (Equation Expr))]
abcBuggyRules = map (siblingOf abcFormula) [ minusB, twoA, minus4AC, oneSolution ]

abcMisconception :: (String -> Rational -> Rational -> Rational -> [OrList (Equation Expr)])
                 -> Transformation (OrList (Equation Expr))
abcMisconception f = makeTransList $ 
   oneDisjunct $ \(lhs :==: rhs) -> do
      guard (rhs == 0)
      (x, (a, b, c)) <- matchM (polyNormalForm rationalView >>> second quadraticPolyView) lhs
      f x a b c
      
minusB :: Rule (OrList (Equation Expr))
minusB = buggyRule $ makeRule "abc.minus-b" $ 
   abcMisconception $ \x a b c -> do
      let discr = sqrt (fromRational (b*b - 4 * a * c))
          f (?) buggy = 
             let minus = if buggy then id else negate
             in Var x :==: (minus (fromRational b) ? discr) / (2 * fromRational a) 
      [ orList [ f (+) True,  f (-) True  ],
        orList [ f (+) False, f (-) True  ],
        orList [ f (+) True,  f (-) False ]]
        
         
twoA :: Rule (OrList (Equation Expr))
twoA = buggyRule $ makeRule "abc.two-a" $ 
   abcMisconception $ \x a b c -> do
      let discr = sqrt (fromRational (b*b - 4 * a * c))
          f (?) buggy = 
             let twice = if buggy then id else (2*)
             in Var x :==: (-fromRational b ? discr) / twice (fromRational a) 
      [ orList [ f (+) True,  f (-) True  ],
        orList [ f (+) False, f (-) True  ],
        orList [ f (+) True,  f (-) False ]]
         
minus4AC :: Rule (OrList (Equation Expr))
minus4AC = buggyRule $ makeRule "abc.minus-4ac" $ 
   abcMisconception $ \x a b c -> do
      let discr (?) = sqrt (fromRational ((b*b) ? (4 * a * c)))
          f (?) buggy = 
             let op = if buggy then (+) else (-)
             in Var x :==: (-fromRational b ? discr op) / (2 * fromRational a)
      [ orList [ f (+) True,  f (-) True  ],
        orList [ f (+) False, f (-) True  ],
        orList [ f (+) True,  f (-) False ]]
         
oneSolution :: Rule (OrList (Equation Expr))
oneSolution = buggyRule $ makeRule "abc.one-solution" $ 
   abcMisconception $ \x a b c -> do
      let discr = sqrt (fromRational (b*b - 4 * a * c))
          f (?) = Var x :==: (-fromRational b ? discr) / (2 * fromRational a)
      [ return $ f (+), return $ f (-) ]