module Domain.Math.Polynomial.LinearEquations 
  ( linearRules, linearStrategy
  , merge, distribute
  , testAll
  ) where 

import Prelude hiding (repeat)
import Common.Apply
import Common.Context
import Common.Strategy hiding (not, fail)
import Common.Transformation
import Common.Uniplate
import Domain.Math.Data.Equation
import Common.Exercise
import Domain.Math.Equation.CoverUpRules
import Domain.Math.ExercisesDWO (linearEquations)
import Domain.Math.Polynomial.Views
import Domain.Math.Expr
import Domain.Math.Simplification (smartConstructors)
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Parser ()
import Domain.Math.View.Basic
import Domain.Math.ExercisesDWO
import Control.Monad (guard)
import Data.List  (partition)
import Data.Maybe (catMaybes)

------------------------------------------------------------
-- Strategy

linearStrategy :: LabeledStrategy (Equation Expr)
linearStrategy = cleanUpStrategy cleanUp $
   label "Linear Equation" 
    $  label "Phase 1" (repeat (removeDivision <|> ruleOnce distribute <|> ruleMulti merge))
   <*> label "Phase 2" (try varToLeft <*> try coverUpPlus <*> try (coverUpTimes |> coverUpNegate))

cleanUp :: Equation Expr -> Equation Expr
cleanUp = fmap (smartConstructors . transform (simplify rationalView))

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
         _ -> Nothing
   f _ = Nothing

mergeT :: Transformation Expr
mergeT = makeTrans "merge" $ return . simplifyWith f sumView
 where
   f = normalizeSum . map (simplifyWith (second normalizeProduct) productView)

-------------------------------------------------------
-- Rewrite Rules

linearRules :: [Rule (Context (Equation Expr))]
linearRules = map ignoreContext
   [ removeDivision, ruleMulti merge, ruleOnce distribute
   , varToLeft, conToRight, scaleToOne
   ]

varToLeft :: Rule (Equation Expr)
varToLeft = makeRule "variable to left" $ flip supply1 minusT $ \eq -> do
   (x, a, _) <- match (linearViewWith rationalView) (getRHS eq)
   guard (a/=0)
   return (fromRational a * variable x)

conToRight :: Rule (Equation Expr)
conToRight = makeRule "constant to right" $ flip supply1 minusT $ \eq -> do
   (_, _, b) <- match (linearViewWith rationalView) (getLHS eq)
   guard (b/=0)
   return (fromRational b)

scaleToOne :: Rule (Equation Expr)
scaleToOne = makeRule "scale to one" $ flip supply1 divisionT $ \eq -> do
   (_, a, _) <- match (linearViewWith rationalView) (getLHS eq)
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
   
distribute :: Rule Expr
distribute = makeSimpleRuleList "distribution" $
   somewhereM (\x -> applyM distributionT x >>= applyM mergeT)

merge :: Rule Expr
merge = makeSimpleRule "merge similar terms" $ \old -> do
   new <- apply mergeT old
   guard (old /= new)
   return new   


----------------------------------------------------------------------
-- Expr normalization
   {-
normalizeExpr :: Expr -> Expr
normalizeExpr a =
   case (match sumView a, match productView a) of
      (Just xs, _) | length xs > 1 -> 
         build sumView (sort $ normalizeSum $ map normalizeExpr xs)
      (_, Just (b, ys)) | length (filter (/= 1) ys) > 1 -> 
         build productView (b, sort $ normalizeProduct $ map normalizeExpr ys)
      _ -> a 
-}
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
 
solveAndCheck :: Equation Expr -> Equation Expr
solveAndCheck eq = 
   case applyD linearStrategy eq of
      Var x :==: e | x `notElem` collectVars e -> 
         let sub y =  if x==y then e else Var y
         in fmap (simplify rationalView . substituteVars sub) eq
      _ -> eq
 
----------------------------------------------------------------------
-- Linear Equations Exercise Sets (from DWO environment)

testAll = all f (concat linearEquations)
 where f eq = case solveAndCheck eq of
                 a :==: b -> if a==b then True else error (show (eq, a, b))
                 
q n = putStrLn $ showDerivationWith show (unlabel $ ignoreContext linearStrategy) $ 
   let x=Var "x" in
   concat linearEquations !! n

go :: IO () 
go = mapM_ f $ zip [0..] (concat linearEquations)
 where 
   f (n, eq) = mapM_ g (applyAll linearStrategy eq)
    where
      g result = 
         let p (x :==: y) = x == Var "x" && y `belongsTo` rationalView
         in if p result then putStr (show n++" ok; ") 
            else error $ show result ++ " for " ++ show n