module Domain.Math.LinearEquations (linearEquationExercise, testAll) where

import Prelude hiding (repeat)
import Common.Apply
import Common.Context
import Common.Exercise
import Common.Utils (distinct, fixpoint)
import qualified Common.Parsing as P
import Common.Strategy hiding (not, fail)
import Common.Transformation
import Common.Uniplate
import Domain.Math.Equation
import Domain.Math.ExercisesDWO (linearEquations)
import Domain.Math.Expr
import Domain.Math.Fraction (cleanUpStrategy)
import Domain.Math.Symbolic
import Domain.Math.Parser
import Domain.Math.Views
import Domain.Math.HigherDegreeEquations (powerView, constantView, ratioView, showDerivation) -- to reuse some helpers (temp)
import Control.Monad
import Data.List hiding (repeat)
import Data.Maybe
import Data.Ratio
import Test.QuickCheck hiding (label)

paper = 
   let start = inContext $ last $ concat linearEquations in
   case derivations (unlabel solveEquation) start of
      hd:_ -> showDerivation "" hd
      _    -> putStrLn "unsolved"

------------------------------------------------------------
-- Exercise

linearEquationExercise :: Exercise (Equation Expr)
linearEquationExercise = makeExercise 
   { identifier    = "lineq"
   , domain        = "math"
   , description   = "solve a linear equation"
   , status        = Experimental
   , parser        = parseLineq
   , equality      = \a b -> a == b
   , equivalence   = viewEquivalent equationView
   , finalProperty = solvedEquation
   , ruleset       = lineqRules
   , strategy      = solveEquation
   , generator     = oneof (map return (concat linearEquations))
   }

parseLineq :: String -> Either P.SyntaxError (Equation Expr)
parseLineq = f . P.parse (pEquation pExpr) . P.scanWith myScanner
 where 
   myScanner = scannerExpr {P.keywordOperators = "==" : P.keywordOperators scannerExpr }
 
   f (e, []) = Right e
   f (_, xs) = Left $ P.ErrorMessage $ unlines $ map show xs

------------------------------------------------------------
-- Strategy

solveEquation :: LabeledStrategy (Context (Equation Expr))
solveEquation = liftF $ 
   label "linear Equation" $ 
   repeat (distribute <|> removeDivision <|> merge) 
      <*> try varToLeft <*> try conToRight <*> try scaleToOne

liftF :: Lift f => f a -> f (Context a)
liftF = lift $ makeLiftPair (return . fromContext) (fmap . const)

-------------------------------------------------------
-- Transformations

plusT, minusT, timesT, divisionT :: Expr -> Equation Expr -> Equation Expr
plusT     e = merge2 . fmap (.+. e)
minusT    e = merge2 . fmap (.-. e)
timesT    e = merge2 . applyD distribute . fmap (.*. e)
divisionT e = merge2 . fmap (./. e)

distributionT :: Expr -> Maybe Expr
distributionT (a :*: b) | length bs > 1 = do
   ratioView a -- restriction
   return $ foldr1 (+) (map (a*) bs)
 where bs = fromMaybe [] (match sumView b)
distributionT (a :*: b) | length as > 1 = do
   ratioView b -- restriction
   return $ foldr1 (+) (map (b*) as)
   
 where as = fromMaybe [] (match sumView a)
distributionT _ = Nothing

-------------------------------------------------------
-- Rewrite Rules

lineqRules :: [Rule (Context (Equation Expr))]
lineqRules = map liftF
   [ removeDivision, merge, distribute
   , varToLeft, conToRight, scaleToOne
   ]

varToLeft :: Rule (Equation Expr)
varToLeft = makeSimpleRule "varToLeft" $ \eq -> do
   (a, x, _) <- match linearView (getRHS eq)
   guard (a/=0)
   return $
      if (a > 0) then minusT (fromRational a       * variable x) eq
                 else plusT  (fromRational (abs a) * variable x) eq

conToRight :: Rule (Equation Expr)
conToRight = makeSimpleRule "conToRight" $ \eq -> do
   (_, _, b) <- match linearView (getLHS eq)
   guard (b/=0)
   return $
      if (b > 0) then minusT (fromRational b) eq
                 else plusT  (fromRational (abs b)) eq

scaleToOne :: Rule (Equation Expr)
scaleToOne = makeSimpleRule "scaleToOne" $ \eq -> do
   (a, _, _) <- match linearView (getLHS eq)
   guard (a `notElem` [0, 1])
   return $ 
      divisionT (fromRational a) eq

removeDivision :: Rule (Equation Expr)
removeDivision = makeSimpleRule "removeDivision" $ \eq@(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   let list = catMaybes (map f (xs ++ ys))
       cons = catMaybes (map constantView list) 
   case (cons, list) of
      (_, [])   -> Nothing
      ([], e:_) -> return $ timesT e eq
      (ns, _)   -> return $ timesT (fromInteger (foldr1 lcm ns)) eq
 where
   f (Negate a) = f a
   f (a :/: b) = do
      guard (constantView a == Nothing)
      constantView b
      return b
   f _ = Nothing

merge :: Rule (Equation Expr)
merge = makeSimpleRule "merge" mergeR

distribute :: Rule (Equation Expr)
distribute = makeSimpleRule "distribute" $ \(lhs :==: rhs) -> 
   let f = somewhereM distributionT in
   case (f lhs,f rhs) of
      (Just new, _) -> Just (new :==: rhs)
      (_, Just new) -> Just (lhs :==: new)
      _  -> Nothing

normalizeProduct :: [Expr] -> [Expr]
normalizeProduct ys = f [ (match rationalView y, y) | y <- ys ]
  where  f []                    = []
         f ((Nothing  , e):xs)   = e:f xs
         f ((Just r   , _):xs)   = 
           let  cs    = r :  [ c  | (Just c   , _)  <- xs ]
                rest  =      [ x  | (Nothing  , x)  <- xs ]
           in   build rationalView (product cs):rest

normalizeSum :: [Expr] -> [Expr]
normalizeSum xs = rec [ (Just $ pm x, x) | x <- xs ]
 where
   pm :: Expr -> (Rational, Expr)
   pm (e1 :*: e2) = case (match rationalView e1, match rationalView e2) of
                       (Just r1, _) -> let (r2, a) = pm e2 in (r1*r2, a)
                       (_, Just r1) -> let (r2, a) = pm e1 in (r1*r2, a) 
                       _           -> (1, e1 .*. e2)
   pm (Negate e) = let (r, a) = pm e in (negate r, a)
   pm e = case match rationalView e of
             Just r  -> (r, Nat 1)
             Nothing -> (1, e)
   
   rec [] = []
   rec ((Nothing, e):xs) = e:rec xs
   rec ((Just (r, a), e):xs) = new:rec rest
    where
      (js, rest) = partition (maybe False ((==a) . snd) . fst) xs
      rs  = r:map fst (catMaybes (map fst js))
      new | null js   = e
          | otherwise = build rationalView (sum rs) .*. a

mergeR e1 = if e1==e2 then Nothing else Just e2
 where e2 = merge2 e1

merge2 e = fmap mergeE e
mergeE a = fromMaybe a $ do 
   xs <- match sumView a
   ys <- flip mapM xs $ \b -> do
            (n, zs) <- match productView b
            return (build productView (n, normalizeProduct zs))
   return (build sumView (normalizeSum ys))

----------------------------------------------------------------------
-- Substitution (for checking) 

substitute :: [(String, Expr)] -> Expr -> Expr
substitute sub = rec 
 where
   rec e@(Var s) = fromMaybe e (lookup s sub)
   rec e = f (map rec cs)
    where (cs, f) = uniplate e

solvedEquation :: Equation Expr -> Bool
solvedEquation (lhs :==: rhs) =
   case lhs of 
      Var x -> x `notElem` collectVars rhs
      _     -> False
 
solveAndCheck :: Equation Expr -> Equation Expr
solveAndCheck eq = 
   case fromContext (applyD solveEquation (inContext eq)) of
      Var x :==: e | x `notElem` collectVars e -> 
         fmap (simplify rationalView . substitute [(x, e)]) eq
      _ -> eq
 
----------------------------------------------------------------------
-- Linear Equations Exercise Sets (from DWO environment)

testAll = all f (concat linearEquations)
 where f eq = case solveAndCheck eq of
                 a :==: b -> if a==b then True else error (show (eq, a, b))
{-
main :: IO ()
main = flip mapM_ [ (level, i) | level <- [1..5], i <- [1..10] ] $ \(level, i) -> do
   let line  = putStrLn (replicate 50 '-')  
       start = (linearEquations !! (level-1)) !! (i-1)
   line
   putStrLn $ "Exercise " ++ show i ++ " (level " ++ show level ++ ")"
   line 
   case derivations (unlabel linearEquationStrategy) start of
      hd:_ -> showDerivation "" hd
      _    -> putStrLn "unsolved" -}