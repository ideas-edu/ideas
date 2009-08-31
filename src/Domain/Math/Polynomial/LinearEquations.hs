module Domain.Math.Polynomial.LinearEquations 
  ( solveEquation, solvedEquation, merge 
  , minusT, timesT, divisionT, distributionT, distribute, mergeT
  , lineqRules, linearEquations
  ) where 

import Prelude hiding (repeat)
import Common.Apply
import Common.Context
import Common.Strategy hiding (not, fail)
import Common.Transformation
import Common.Uniplate
import Domain.Math.Data.Equation
import Domain.Math.ExercisesDWO (linearEquations)
import Domain.Math.Expr
import Domain.Math.Simplification (smartConstructors)
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Parser ()
import Domain.Math.View.Basic
import Control.Monad (guard)
import Data.List  (partition, sort)
import Data.Maybe (catMaybes)

------------------------------------------------------------
-- Strategy

solveEquation :: LabeledStrategy (Context (Equation Expr))
solveEquation = ignoreContext $ cleanUpStrategy (fmap smartConstructors) $
   label "Linear Equation" 
    $  label "Phase 1" (repeat (removeDivision <|> distribute <|> merge))
   <*> label "Phase 2" (try varToLeft <*> try conToRight <*> try scaleToOne)

-------------------------------------------------------
-- Transformations

plusT, minusT :: Expr -> Transformation (Equation Expr)
plusT  e = makeTrans "plus"  $ return . fmap (applyD mergeT . (.+. e))
minusT e = makeTrans "minus" $ return . fmap (applyD mergeT . (.-. e))

timesT :: Expr -> Transformation (Equation Expr)
timesT e = makeTrans "times" $ \eq -> do 
   r <- match rationalView e
   guard (r /= 0)
   return $ fmap (applyD mergeT . applyD distributionT . (e .*.)) eq

divisionT :: Expr -> Transformation (Equation Expr)
divisionT e = makeTrans "division" $ \eq -> do
   r <- match rationalView e
   guard (r /= 0)
   return $ fmap (applyD mergeT . applyD distributionT . (./. e)) eq

distributionT :: Transformation Expr
distributionT = makeTrans "distribute" f 
 where
   f (a :*: b) =
      case (match sumView a, match sumView b) of
         (Just as, Just bs) | length as > 1 || length bs > 1 -> 
            return $ build sumView [ (a .*. b) | a <- as, b <- bs ]
{-         (Just as, _) | length as > 1 ->
            return $ build sumView (map (.*. b) as) 
         (_, Just bs) | length bs > 1 -> 
            return $ build sumView (map (a .*.) bs) -}
         _ -> Nothing
   f _ = Nothing

mergeT :: Transformation Expr
mergeT = makeTrans "merge" $ return . simplifyWith f sumView
 where
   f = normalizeSum . map (simplifyWith (second normalizeProduct) productView)

-------------------------------------------------------
-- Rewrite Rules

lineqRules :: [Rule (Context (Equation Expr))]
lineqRules = map ignoreContext
   [ removeDivision, merge, distribute
   , varToLeft, conToRight, scaleToOne
   ]

varToLeft :: Rule (Equation Expr)
varToLeft = makeRule "variable to left" $ flip supply1 minusT $ \eq -> do
   (a, x, _) <- match linearView (getRHS eq)
   guard (a/=0)
   return (fromRational a * variable x)

conToRight :: Rule (Equation Expr)
conToRight = makeRule "constant to right" $ flip supply1 minusT $ \eq -> do
   (_, _, b) <- match linearView (getLHS eq)
   guard (b/=0)
   return (fromRational b)

scaleToOne :: Rule (Equation Expr)
scaleToOne = makeRule "scale to one" $ flip supply1 divisionT $ \eq -> do
   (a, _, _) <- match linearView (getLHS eq)
   guard (a `notElem` [0, 1])
   return (fromRational a)

removeDivision :: Rule (Equation Expr)
removeDivision = makeRule "remove division" $ flip supply1 timesT $ \(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   zs <- mapM (fmap snd . match productView) (filter hasVars (xs ++ ys))
   let f = fmap snd . match (divView >>> second integerView)
   case catMaybes (map f (concat zs)) of
      [] -> Nothing
      ns -> return (fromInteger (foldr1 lcm ns))

distribute :: Rule (Equation Expr)
distribute = makeSimpleRuleList "distribution" $ \(lhs :==: rhs) -> 
   let f = somewhereM (\x -> applyM distributionT x >>= applyM mergeT)
   in [ new :==: rhs | new <- f lhs ] ++
      [ lhs :==: new | new <- f rhs ]

merge :: Rule (Equation Expr)
merge = makeSimpleRule "merge similar terms" $ \old -> do
   let new = fmap (applyD mergeT) old
   guard (old /= new)
   return new   

----------------------------------------------------------------------
-- Expr normalization
   
normalizeExpr :: Expr -> Expr
normalizeExpr a =
   case (match sumView a, match productView a) of
      (Just xs, _) | length xs > 1 -> 
         build sumView (sort $ normalizeSum $ map normalizeExpr xs)
      (_, Just (b, ys)) | length (filter (/= 1) ys) > 1 -> 
         build productView (b, sort $ normalizeProduct $ map normalizeExpr ys)
      _ -> a

normalizeProduct :: [Expr] -> [Expr]
normalizeProduct ys = f [ (match rationalView y, y) | y <- ys ]
  where  f []                    = []
         f ((Nothing  , e):xs)   = e:f xs
         f ((Just r   , _):xs)   = 
           let  cs    = r :  [ c  | (Just c   , _)  <- xs ]
                rest  =      [ x  | (Nothing  , x)  <- xs ]
           in   build rationalView (product cs):rest

normalizeSum :: [Expr] -> [Expr]
normalizeSum xs = rec [ (Just $ pm 1 x, x) | x <- xs ]
 where
   pm :: Rational -> Expr -> (Rational, Expr)
   pm r (e1 :*: e2) = case (match rationalView e1, match rationalView e2) of
                         (Just r1, _) -> pm (r*r1) e2
                         (_, Just r1) -> pm (r*r1) e1
                         _           -> (r, e1 .*. e2)
   pm r (Negate e) = pm (negate r) e
   pm r e = case match rationalView e of
               Just r1 -> (r*r1, Nat 1)
               Nothing -> (r, e)
   
   rec [] = []
   rec ((Nothing, e):xs) = e:rec xs
   rec ((Just (r, a), e):xs) = new:rec rest
    where
      (js, rest) = partition (maybe False ((==a) . snd) . fst) xs
      rs  = r:map fst (catMaybes (map fst js))
      new | null js   = e
          | otherwise = build rationalView (sum rs) .*. a
          
----------------------------------------------------------------------
-- Substitution (for checking) 

solvedEquation :: Equation Expr -> Bool
solvedEquation (Var x :==: rhs) = x `notElem` collectVars rhs
solvedEquation _                = False
 
solveAndCheck :: Equation Expr -> Equation Expr
solveAndCheck eq = 
   case fromContext (applyD solveEquation (inContext eq)) of
      Var x :==: e | x `notElem` collectVars e -> 
         let sub y =  if x==y then e else Var y
         in fmap (simplify rationalView . substituteVars sub) eq
      _ -> eq
 
----------------------------------------------------------------------
-- Linear Equations Exercise Sets (from DWO environment)

testAll = all f (concat linearEquations)
 where f eq = case solveAndCheck eq of
                 a :==: b -> if a==b then True else error (show (eq, a, b))