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

import Domain.Math.Expr
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.Rules (abcFormula)
import Domain.Math.Numeric.Views
import Common.View
import Common.Transformation
import Common.Traversable
import Control.Monad

buggyRulesEquation :: [Rule (Equation Expr)]
buggyRulesEquation = 
   [ buggyPlus, buggyNegateOneSide, buggyFlipNegateOneSide, buggyNegateAll
   , buggyDivNegate, buggyDivNumDenom 
   , ruleOnce (ruleSomewhere buggyDistrTimes)
   ]

buggyPlus :: Rule (Equation Expr)
buggyPlus = describe "Moving a term from the left-hand side to the \
   \right-hand side (or the other way around), but forgetting to change \
   \the sign." $ 
   buggyRule $ makeSimpleRuleList "buggy plus" $ \(lhs :==: rhs) -> do
      (a, b) <- matchM plusView lhs
      [ a :==: rhs + b, b :==: rhs + a ]
    `mplus` do
      (a, b) <- matchM plusView rhs
      [ lhs + a :==: b, lhs + b :==: a ]

buggyNegateOneSide :: Rule (Equation Expr)
buggyNegateOneSide = describe "Negate terms on one side only." $
   buggyRule $ makeSimpleRuleList "buggy negate one side" $ \(lhs :==: rhs) -> do
      [ -lhs :==: rhs, lhs :==: -rhs  ] 

buggyFlipNegateOneSide :: Rule (Equation Expr)
buggyFlipNegateOneSide = describe "Negate terms on one side only." $
   buggyRule $ makeSimpleRuleList "buggy flip negate one side" $ \(lhs :==: rhs) -> do
      [ -rhs :==: lhs, rhs :==: -lhs  ]

buggyNegateAll :: Rule (Equation Expr)
buggyNegateAll = describe "Negating all terms (on both sides of the equation, \
   \but forgetting one term." $
   buggyRule $ makeSimpleRuleList "buggy negate all" $ \(lhs :==: rhs) -> do 
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
   buggyRule $ makeSimpleRuleList "buggy divide negate" $ \(lhs :==: rhs) -> do
      (a, b) <- matchM timesView lhs
      [ b :==: rhs/(-a) | noVars a ] ++ [ a :==: rhs/(-b) | noVars b ]
    `mplus` do
      (a, b) <- matchM timesView rhs
      [ lhs/(-a) :==: b | noVars a ] ++ [ lhs/(-b) :==: a | noVars b ]

buggyDivNumDenom :: Rule (Equation Expr)
buggyDivNumDenom = describe "Dividing, but flipping numerator/denominator." $
   buggyRule $ makeSimpleRuleList "buggy divide numdenom" $ \(lhs :==: rhs) -> do
      (a, b) <- matchM timesView lhs
      [ b :==: a/rhs | noVars rhs ] ++ [ a :==: b/rhs | noVars rhs ]
    `mplus` do
      (a, b) <- matchM timesView rhs
      [ a/lhs :==: b | noVars lhs ] ++ [ b/lhs :==: a | noVars lhs ]

buggyDistrTimes :: Rule Expr
buggyDistrTimes = describe "Incorrect distribution of times over plus." $
   buggyRule $ makeSimpleRuleList "buggy distr times plus" $ \expr -> do
      (a, (b, c)) <- matchM (timesView >>> second plusView) expr
      [ a*b+c, b+a*c ]
    `mplus` do
      ((a, b), c) <- matchM (timesView >>> first plusView) expr
      [ a*c+b, a+b*c ]

---------------------------------------------------------
-- ABC formula misconceptions

abcBuggyRules :: [Rule (OrList (Equation Expr))]
abcBuggyRules = map f [ minusB, twoA, minus4AC, oneSolution ]
 where
   f r = r { ruleSiblings = [name abcFormula] }

abcMisconception :: (String -> Rational -> Rational -> Rational -> [OrList (Equation Expr)])
                 -> Transformation (OrList (Equation Expr))
abcMisconception f = makeTransList $ 
   onceJoinM $ \(lhs :==: rhs) -> do
      guard (rhs == 0)
      (x, (a, b, c)) <- matchM (polyNormalForm rationalView >>> second quadraticPolyView) lhs
      f x a b c
      
minusB :: Rule (OrList (Equation Expr))
minusB = buggyRule $ makeRule "abc misconception minus b" $ 
   abcMisconception $ \x a b c -> do
      let discr = sqrt (fromRational (b*b - 4 * a * c))
          f (?) buggy = 
             let minus = if buggy then id else negate
             in Var x :==: (minus (fromRational b) ? discr) / (2 * fromRational a) 
      [ orList [ f (+) True,  f (-) True  ],
        orList [ f (+) False, f (-) True  ],
        orList [ f (+) True,  f (-) False ]]
        
         
twoA :: Rule (OrList (Equation Expr))
twoA = buggyRule $ makeRule "abc misconception two a" $ 
   abcMisconception $ \x a b c -> do
      let discr = sqrt (fromRational (b*b - 4 * a * c))
          f (?) buggy = 
             let twice = if buggy then id else (2*)
             in Var x :==: (-fromRational b ? discr) / twice (fromRational a) 
      [ orList [ f (+) True,  f (-) True  ],
        orList [ f (+) False, f (-) True  ],
        orList [ f (+) True,  f (-) False ]]
         
minus4AC :: Rule (OrList (Equation Expr))
minus4AC = buggyRule $ makeRule "abc misconception minus 4ac" $ 
   abcMisconception $ \x a b c -> do
      let discr (?) = sqrt (fromRational ((b*b) ? (4 * a * c)))
          f (?) buggy = 
             let op = if buggy then (+) else (-)
             in Var x :==: (-fromRational b ? discr op) / (2 * fromRational a)
      [ orList [ f (+) True,  f (-) True  ],
        orList [ f (+) False, f (-) True  ],
        orList [ f (+) True,  f (-) False ]]
         
oneSolution :: Rule (OrList (Equation Expr))
oneSolution = buggyRule $ makeRule "abc misconception one solution" $ 
   abcMisconception $ \x a b c -> do
      let discr = sqrt (fromRational (b*b - 4 * a * c))
          f (?) = Var x :==: (-fromRational b ? discr) / (2 * fromRational a)
      [ return $ f (+), return $ f (-) ]