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
-----------------------------------------------------------------------------
module Domain.Math.Polynomial.RationalExercises 
   ( rationalEquationExercise
   , simplifyRationalExercise, divisionRationalExercise
   ) where

import Common.Classes
import Common.Context
import Common.Exercise
import Common.Navigator
import Common.Rewriting.Term (Term)
import Common.Strategy hiding (not)
import Common.TestSuite
import Common.Transformation
import Common.Uniplate hiding (somewhere)
import Common.View
import Control.Monad
import Data.List hiding (repeat)
import Data.Maybe
import Data.Ratio
import Domain.Math.Clipboard
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Equation.Views
import Domain.Math.Examples.DWO4
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Exercises
import Domain.Math.Polynomial.Exercises (eqOrList)
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Strategies
import Domain.Math.Polynomial.Views
import Domain.Math.Power.Views
import Prelude hiding (repeat, until, (^))
import Test.QuickCheck hiding (label)
import qualified Domain.Logic.Formula as Logic

go0 = checkExercise rationalEquationExercise
go2 = checkExercise simplifyRationalExercise

see n = printDerivation simplifyRationalExercise (examples simplifyRationalExercise !! (n-1))
       
go4 = printDerivation findFactorsExercise $ -a + 4
 where x = Var "x"
       a = Var "a"

rationalEquationExercise :: Exercise (OrList (Equation Expr))
rationalEquationExercise = makeExercise 
   { exerciseId   = describe "solve a rational equation (with a variable in a divisor)" $ 
                       newId "math.rationaleq"
   , status       = Alpha -- Provisional
   , parser       = parseExprWith (pOrList (pEquation pExpr))
   , isReady      = solvedRelations
--   , equivalence  = ??
   , similarity   = eqOrList cleanUpExpr2
   , strategy     = rationalEquationStrategy
   , ruleOrdering = ruleOrderingWithId quadraticRuleOrder
   , navigation   = termNavigator
   , examples     = map return (concat brokenEquations)
   }
   
simplifyRationalExercise :: Exercise Expr
simplifyRationalExercise = makeExercise
   { exerciseId   = describe "simplify a rational expression (with a variable in a divisor)" $ 
                       newId "math.simplifyrational"
   , status       = Alpha -- Provisional
   , parser       = parseExpr
   , isReady      = isNormBroken
--   , equivalence  = ??
   , similarity   = \x y -> cleanUpExpr2 x == cleanUpExpr2 y
   , strategy     = simplifyRationalStrategy
   , navigation   = termNavigator
   , examples     = concat (normBroken ++ normBroken2)
   }
   
divisionRationalExercise :: Exercise Expr
divisionRationalExercise = simplifyRationalExercise
   { exerciseId   = describe "divide a rational expression ('uitdelen')" $ 
                       newId "math.divrational"
   , strategy     = label "divide broken fraction" succeed
   , examples     = concat deelUit
   }

rationalEquationStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
rationalEquationStrategy = cleanUpStrategy (applyTop (fmap (fmap cleanUpExpr2))) $
   label "Rational equation" $ 
       brokenFormToPoly <*> higherDegreeStrategyG
 where
   brokenFormToPoly = label "rational form to polynomial" $ until allArePoly $
      (  useC divisionIsZero <|> useC divisionIsOne 
     <|> useC sameDivisor <|> useC sameDividend
     <|> use coverUpPlus <|> use coverUpMinusLeft <|> use coverUpMinusRight
     <|> use coverUpNegate
      ) |>    
      (  useC crossMultiply <|> useC multiplyOneDiv  )

allArePoly :: Context (OrList (Equation Expr)) -> Bool
allArePoly = 
   let f a = a `belongsTo` polyView
   in maybe False (all f . concatMap crush . crush) .  fromContext

simplifyRationalStrategy :: LabeledStrategy (Context Expr)
simplifyRationalStrategy = cleanUpStrategy (applyTop cleanUpExpr2) $
   label "Simplify rational expression" $
      phaseOneDiv <*> phaseSimplerDiv
 where
   phaseOneDiv = label "Write as division" $
      until isDivC $ 
         use fractionPlus <|> use fractionScale <|> use turnIntoFraction
   phaseSimplerDiv = label "Simplify division" $
      repeat $
         (onlyInLowerDiv findFactorsStrategyG <|> somewhere (useC cancelTermsDiv)
            <|> commit (onlyInUpperDiv (repeat findFactorsStrategyG) <*> useC cancelTermsDiv))
         |> ( somewhere (use merge) 
         <|> multi (showId distributeTimes) (notInLowerDiv (use distributeTimes))
          )

isDivC :: Context a -> Bool
isDivC = maybe False (isJust . isDivide :: Term -> Bool) . currentT

-- First check that the whole strategy can be executed. Cleaning up is not 
-- propagated correctly to predicate in check combinator, hence the use of
-- cleanUpStrategy (which is not desirable here).
commit :: IsStrategy f => f (Context Expr) -> Strategy (Context Expr)
commit s = let cs  = cleanUpStrategy (applyTop cleanUpExpr2) (label "" s)
               f a = fromMaybe a (do b <- top a; c <- current a; return (change (const c) b))
           in check (applicable cs . f) <*> s

-- copy/paste from Strategy.Combinators
notInLowerDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
notInLowerDiv s = fix $ \this -> s <|> once this
 where
   once s = ruleMoveDown <*> s <*> ruleMoveUp
   ruleMoveDown = minorRule $ makeSimpleRuleList "MoveDownNotLower" $ \c ->
      if (isDivC c) then (down 1 c) else allDowns c
   ruleMoveUp   = minorRule $ makeSimpleRule "MoveUp" safeUp
   safeUp a     = maybe (Just a) Just (up a)

onlyInLowerDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
onlyInLowerDiv s = check isDivC <*> ruleMoveDown <*> s <*> ruleMoveUp
 where
   ruleMoveDown = minorRule $ makeSimpleRuleList "MoveDown2" (down 2)
   ruleMoveUp   = minorRule $ makeSimpleRule "MoveUp" safeUp
   safeUp a     = maybe (Just a) Just (up a)

onlyInUpperDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
onlyInUpperDiv s = check isDivC <*> ruleMoveDown <*> s <*> ruleMoveUp
 where
   ruleMoveDown = minorRule $ makeSimpleRuleList "MoveDown1" (down 1)
   ruleMoveUp   = minorRule $ makeSimpleRule "MoveUp" safeUp
   safeUp a     = maybe (Just a) Just (up a)

-- a/b = 0  iff  a=0 (and b/=0)
divisionIsZero :: Rule (Context (Equation Expr))
divisionIsZero = makeSimpleRule "divisionIsZero" $ withCM $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (a, b) <- matchM divView lhs
   conditionNotZero b
   return (a :==: 0)
   
-- a/b = 1  iff  a=b (and b/=0)
divisionIsOne :: Rule (Context (Equation Expr))
divisionIsOne = makeSimpleRule "divisionIsOne" $ withCM $ \(lhs :==: rhs) -> do
   guard (rhs == 1)
   (a, b) <- matchM divView lhs
   conditionNotZero b
   return (a :==: b)

-- a/c = b/c  iff  a=b (and c/=0)
sameDivisor :: Rule (Context (Equation Expr))
sameDivisor = makeSimpleRule "sameDivisor" $ withCM $ \(lhs :==: rhs) -> do
   (a, c1) <- matchM divView lhs
   (b, c2) <- matchM divView rhs
   guard (c1==c2)
   conditionNotZero c1
   return (a :==: b)
   
-- a/b = a/c  iff  a=0 or b=c (and b/=0 and c/=0)
sameDividend :: Rule (Context (OrList (Equation Expr)))
sameDividend = makeSimpleRule "sameDividend" $ withCM $ oneDisjunct $ \(lhs :==: rhs) -> do
   (a1, b) <- matchM divView lhs
   (a2, c) <- matchM divView rhs
   guard (a1==a2)
   conditionNotZero b
   conditionNotZero c
   return $ orList [a1 :==: 0, b :==: c]
   
-- a/b = c/d  iff  a*d = b*c   (and b/=0 and d/=0)
crossMultiply :: Rule (Context (Equation Expr))
crossMultiply = makeSimpleRule "crossMultiply" $ withCM $ \(lhs :==: rhs) -> do
   (a, b) <- matchM divView lhs
   (c, d) <- matchM divView rhs
   conditionNotZero b
   conditionNotZero d
   return (a*d :==: b*c)
   
-- a/b = c  iff  a = b*c  (and b/=0)
multiplyOneDiv :: Rule (Context (Equation Expr))
multiplyOneDiv = makeSimpleRule "multiplyOneDiv" $ withCM $ \(lhs :==: rhs) -> 
   f (:==:) lhs rhs `mplus` f (flip (:==:)) rhs lhs
 where
   f eq ab c = do 
      guard (not (c `belongsTo` divView))
      (a, b) <- matchM divView ab
      conditionNotZero b
      return (a `eq` (b*c))
      
-- a/c + b/c = a+b/c   (also see Numeric.Rules)
fractionPlus :: Rule Expr -- also minus
fractionPlus = makeSimpleRule "fraction plus" $ \expr -> do
   ((a, b), (c, d)) <- match myView expr
   guard (b == d)
   return (build divView (a+c, b))
 where
   myView = plusView >>> (divView *** divView)

-- ab/ac  =>  b/c  (if a/=0)
-- Note that the common term can be squared (in one of the parts)
cancelTermsDiv :: Rule (Context Expr)
cancelTermsDiv = makeSimpleRule "cancel terms div" $ withCM $ \expr -> do
   ((b, xs), (c, ys)) <- matchM myView expr
   let (ps, qs, rs) = rec (map f xs) (map f ys)
   guard (not (null rs))
   conditionNotZero (build productView (False, map g rs))
   return $ build myView ((b, map g ps), (c, map g qs))
 where
   myView = divView >>> (productView *** productView)
   powInt = simplePowerView >>> second integerView
   f a = fromMaybe (a, 1) (match powInt a)
   g a = build powInt a
   rec ((_, 0):xs) ys = rec xs ys
   rec (pair@(a, n):xs) ys =
      case break ((==a) . fst) ys of
         (ys1, (b, m):ys2)
            | m == 0 ->
                 rec (pair:xs) (ys1++ys2)
            | otherwise ->
                 let i = n `min` m 
                     (ps,qs,rs) = rec ((a, n-i):xs) (ys1++(b,m-i):ys2)
                 in (ps, qs, (a,i):rs)
         _ -> 
            let (ps,qs,rs) = rec xs ys 
            in (pair:ps, qs,rs)
   rec xs ys = (xs, ys, [])

fractionScale :: Rule Expr
fractionScale = liftRule myView $ 
   makeSimpleRule "fraction scale" $ \((a, e1), (b, e2)) -> do
      guard (e1 /= e2)
      let e3 = lcmExpr e1 e2
      ma <- divisionExpr e3 e1
      mb <- divisionExpr e3 e2
      guard (ma /= 1 || mb /= 1)
      return $ ((ma*a, e3), (mb*b, e3))
 where
   myView = plusView >>> (divView *** divView)
   
turnIntoFraction :: Rule Expr
turnIntoFraction = liftRule plusView $
   makeSimpleRule "turn into fraction" $ \(a, b) ->
      liftM (\c -> (c, b)) (f a b) `mplus` 
      liftM (\c -> (a, c)) (f b a)
 where
   f a b = do
      guard (not (a `belongsTo` divView))
      (_, e) <- match divView b
      return $ build divView (a*e, e)

isNormBroken :: Expr -> Bool
isNormBroken (Negate a) = isNormBroken a
isNormBroken (a :/: b) = noVarInDivisor a && noVarInDivisor b
isNormBroken e = noVarInDivisor e

noVarInDivisor :: Expr -> Bool
noVarInDivisor expr = and [ noVars a | _ :/: a <- universe expr ]

lcmExpr :: Expr -> Expr -> Expr
lcmExpr a b = fromMaybe (a*b) $ do
   (ar, as) <- match powerProductView a
   (br, bs) <- match powerProductView b
   return $ build powerProductView (lcmR ar br, merge as bs)
 where   
   lcmR :: Rational -> Rational -> Rational
   lcmR r1 r2 = 
      let f r = numerator r * denominator r
      in fromIntegral (lcm (f r1) (f r2))
   
   merge :: [(Expr, Integer)] -> [(Expr, Integer)] -> [(Expr, Integer)]
   merge = foldr op id
    where
      op (e, n1) f ys = 
         let n2   = fromMaybe 0 (lookup e ys)
             rest = filter ((/=e) . fst) ys
         in (e, n1 `max` n2) : f rest

divisionExpr :: Expr -> Expr -> Maybe Expr
divisionExpr a b = do
   (ar, as) <- match powerProductView a
   (br, bs) <- match powerProductView b
   xs       <- as `without` bs
   return $ build powerProductView (ar/br, xs)
 where
   without :: [(Expr, Integer)] -> [(Expr, Integer)] -> Maybe [(Expr, Integer)]
   without [] ys =
      guard (null ys) >> return []
   without ((e,n1):xs) ys = 
      let n2   = fromMaybe 0 (lookup e ys)
          rest = filter ((/=e) . fst) ys
      in liftM ((e,n1-n2):) (without xs rest)

powerProductView :: View Expr (Rational, [(Expr, Integer)])
powerProductView = makeView f g
 where
   f expr = do
      (b, xs) <- match productView expr
      let (r, ys) = collect xs
      return (if b then -r else r, merge ys)
         
   g (r, xs) =
      build productView (False, fromRational r : map (build pvn) xs)
   
   pvn :: View Expr (Expr, Integer)
   pvn = simplePowerView >>> second integerView

   collect :: [Expr] -> (Rational, [(Expr, Integer)])
   collect = foldr op (1, [])
    where
      op e (r, xs) = 
         let mr   = match rationalView e 
             f r2 = (r*r2, xs)
             pair = fromMaybe (e,1) (match pvn e)
         in maybe (r, pair:xs) f mr

   merge :: [(Expr, Integer)] -> [(Expr, Integer)]
   merge [] = []
   merge xs@((e, _) : _) = 
      let (xs1, xs2) = partition ((==e) . fst) xs
          n = sum (map snd xs1) 
      in (e, n) : merge xs2

test = e4
 where 
   a  = Var "a"
   b  = Var "b"
   
   e1 = 6*a*b*a
   e2 = -4*b^2*a*2
   e3 = lcmExpr e1 e2
   e4 = divisionExpr e3 e1
   e5 = divisionExpr e3 e2
   
testLCM :: TestSuite
testLCM = suite "lcmExpr" $ do
   addProperty "transitivity" $ f3 $ \a b c -> 
      lcmExpr a (lcmExpr b c) ~= lcmExpr (lcmExpr a b) c
   addProperty "commutativity" $ f2 $ \a b -> 
      lcmExpr a b ~= lcmExpr b a
   addProperty "idempotency" $ f1 $ \a -> 
      lcmExpr a a ~= absExpr a
   addProperty "zero" $ f1 $ \a -> 
      lcmExpr a 0 ~= 0
   addProperty "one" $ f1 $ \a -> 
      lcmExpr a 1 ~= absExpr a
   addProperty "sign" $ f2 $ \a b -> 
      lcmExpr a b ~= lcmExpr (-a) b
 where 
   f1 g = liftM  g genExpr
   f2 g = liftM2 g genExpr genExpr
   f3 g = liftM3 g genExpr genExpr genExpr
 
   genExpr, genTerm, genAtom :: Gen Expr
   genExpr = do
      n  <- choose (0, 10)
      b  <- arbitrary
      xs <- replicateM n genTerm
      return $ build productView (b, xs)
   
   genTerm = frequency [(3, genAtom), (1, liftM fromInteger arbitrary)]
   
   genAtom = do
      v <- oneof $ map (return . Var) ["a", "b", "c"]
      i <- choose (-10, 10)
      n <- choose (0, 10)
      p <- frequency [(3, return v), (1, return (v .+. fromInteger i))]
      frequency [(3, return p), (1, return (p^fromInteger n))]

   x ~= y = let f = simplifyWith (second sort) powerProductView 
            in f x == f y
   absExpr = simplifyWith (first (const False)) productView
   
condition :: Relation Expr -> ContextMonad ()
condition rel = return () {- do
   mp <- maybeOnClipboardG "condition"
   let f = maybe id (Logic.:&&:) mp
   addToClipboardG "condition" (f (Logic.Var rel)) -}

conditionNotZero :: Expr -> ContextMonad ()
conditionNotZero e = condition (e ./=. 0)

go = putStrLn $ unlines $ map show $ zip [1..] $ map brokenEq (concat brokenEquations)

-- brokenEq :: Equation Expr -> (Expr, [Relation Expr])
brokenEq eq = 
   let (lhs :==: rhs) = doCovers eq
       (a, b, cs) = brokenExpr (lhs .-. rhs)
       as = maybe [a] snd (match productView a)
       bs = maybe [b] snd (match productView b)
       new1 = match higherDegreeEquationsView $ orList $ map (:==: 0) as
       new2 = match higherDegreeEquationsView $ orList $ map conv $ 
                 concatMap notZero bs ++ cs
       conv r = leftHandSide r :==: rightHandSide r
   in (new1, new2)

-- write expression as a/b, under certain conditions
brokenExpr :: Expr -> (Expr, Expr, [Relation Expr])
brokenExpr expr =
   case expr of 
      a :+: b  -> brokenExpr a `fPlus` brokenExpr b
      a :-: b  -> brokenExpr (a :+: Negate b)
      Negate a -> fNeg (brokenExpr a)
      a :*: b  -> brokenExpr a `fTimes` brokenExpr b
      a :/: b  -> brokenExpr a `fTimes` fRecip (brokenExpr b)
      Sym s [a, b] | s == powerSymbol -> 
         fPower (brokenExpr a) b
      Nat _ -> (expr, 1, [])
      Var _ -> (expr, 1, [])
      _ -> error $ show expr
 where
   fNeg   (a, b, cs)   = (neg a, b, cs)
   fRecip (a, b, cs)   = (b, a, notZero b ++ cs)
   fPower (a, b, cs) n = (a .^. n, b .^. n, cs)
   fTimes (a1, a2, acs) (b1, b2, bcs) = (a1 .*. b1, a2 .*. b2, acs++bcs)
   fPlus  (a1, a2, acs) (b1, b2, bcs) =
      let c2 = lcmExpr a2 b2
          cs = acs++bcs
      in case (divisionExpr c2 a2, divisionExpr c2 b2) of 
            (Just a3, Just b3) 
               | a1 == b1     -> (a1 .*. (a3 .+. b3), c2, cs)
               | a1 == neg b1 -> (a1 .*. (a3 .-. b3), c2, cs)
               | otherwise    -> (a1 .*. a3 .+. b1 .*. b3, c2, cs)
            _ -> (a1 .*. b2 .+. b1 .*. a2, a2 .*. b2, cs)

raar = brokenExpr $ x^2/(5*x+6) + 1
 where x = Var "x"

doCovers :: Equation Expr -> Equation Expr
doCovers eq = 
   case [ new | r <- rs, new <- applyAll r eq ] of
      hd:_ -> doCovers hd
      _    -> eq
 where
   rs = [coverUpPlus, coverUpMinusLeft, coverUpMinusRight, coverUpNegate]

notZero (Nat 1) = []
notZero a = [a ./=. 0]