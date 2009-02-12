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
import Domain.Math.HigherDegreeEquations (solvedEquation, powerView, constantView, ratioView, showDerivation) -- to reuse some helpers (temp)
import Control.Monad
import Data.List hiding (repeat)
import Data.Maybe
import Data.Ratio
import Test.QuickCheck (oneof)

------------------------------------------------------------
-- Exercise

linearEquationExercise :: Exercise (Equation Expr)
linearEquationExercise = makeExercise 
   { identifier    = "lineq"
   , domain        = "math"
   , description   = "solve a linear equation"
   , status        = Experimental
   , parser        = parseLineq
   , equality      = \a b -> fmap cleanUpExpr a == fmap cleanUpExpr b
   , equivalence   = (~=)
   , finalProperty = solvedEquation
   , ruleset       = lineqRules
   , strategy      = linearEquationStrategy
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

linearEquationStrategy :: LabeledStrategy (Context (Equation Expr))
linearEquationStrategy = cleanUpStrategy (fmap (fmap cleanUpExpr)) $ label "linear Equation" $ 
            repeat (liftRule noDivision |> liftRule merge |> liftRule distribute) 
        <*> try (liftRule varToLeft)
        <*> try (liftRule conToRight)
        <*> try (liftRule scaleToOne)

liftRule :: Rule a -> Rule (Context a)
liftRule = lift $ makeLiftPair (return . fromContext) (fmap . const)

------------------------------------------------------------
-- Clean up

cleanUpExpr :: Expr -> Expr
cleanUpExpr = fixpoint (transform (\e -> fromMaybe (step e) (basic e)))
 where
   -- plus
   step (x :+: Negate y) = x - y
   step (x :+: (Negate y :*: z)) = x - (y*z)
   -- minus
   step (x :-: (y :-: z)) = (x-y)+z
   step (0 :-: x) = Negate x
   -- negate 
   step (Negate (Nat 0))   = 0
   step (Negate (x :*: y)) = (-x)*y
   -- times
   step (x :*: (y :/: z)) = (x :*: y) :/: z
   -- division
   step ((a :*: x) :/: b) | a==b && b/=0 = x
   step (Negate (x :/: y)) = (-x)/y
   -- finally, propagate constants
   step expr =
      let Nat n = if n >= 0 then Nat n else negate (Nat (abs n)) in
      case ratioView expr of
         Just (a, b)
            | b==1      -> fromInteger a
            | otherwise -> fromInteger a / fromInteger b
         Nothing -> expr

   -- identities/absorbing
   basic :: Expr -> Maybe Expr
   basic (Nat 0 :+: x) = return x
   basic (x :+: Nat 0) = return x
   basic (Nat 0 :*: _) = return 0
   basic (_ :*: Nat 0) = return 0
   basic (Nat 1 :*: x) = return x
   basic (x :*: Nat 1) = return x
   basic (x :-: Nat 0) = return x
   basic (x :/: Nat 1) = return x
   basic (Nat 0 :/: _) = return 0 -- division-by-zero
   basic _ = Nothing

-- ============================================================ --
--  O L D    S T U F F 
-- ============================================================ --

















-- ax + b
linearView :: View Expr (Rational, String, Rational)
linearView = makeView f g
 where 
   f e = do
      list <- match sumView e >>= mapM powerView
      let (as, xs, ns) = unzip3 list
      unless (length (nub xs) == 1 && distinct ns && all (<=1) ns) Nothing
      cs <- mapM ratioView (map cleanUpExpr as)
      let a = maybe 0 f $ lookup 1 (zip ns cs)
          b = maybe 0 f $ lookup 0 (zip ns cs)
          f (i,j) = fromIntegral i/fromIntegral j
      return (a, head xs, b)
   
   g (a, x, b) = build sumView $ firstPart ++ [ fromRational b | b /= 0 ]
    where
      var = variable x
      firstPart 
         | a == -1   = [negate var] 
         | a ==  0   = []
         | a ==  1   = [var]
         | otherwise = [fromRational a*var]

-------------------------------------------------------
-- Transformations

plusT, minusT, timesT, divisionT :: Fractional a => a -> Equation a -> Equation a
plusT     e = fmap (+e)
minusT    e = fmap (\x -> x-e)
timesT    e = fmap (e*) -- order is somehow significant
divisionT e = fmap (/e)

distributionT :: Expr -> Maybe Expr
distributionT (a :*: b) | length bs > 1 = do
   ratioView a -- restriction
   return $ foldr1 (+) (map (a*) bs)
 where bs = fromMaybe [] (match sumView b)
distributionT (a :*: b) | length as > 1 = do
   ratioView a -- restriction
   return $ foldr1 (+) (map (b*) as)
   
 where as = fromMaybe [] (match sumView a)
distributionT _ = Nothing

mergeTerms :: Equation Expr -> Equation Expr
mergeTerms = fmap (cleanUpExpr . merge)
 where
   make x [] = 0
   make x ((a, n):rest) = let (xs, ys) = partition ((==n) . snd) rest
                          in ((if n==1 then (*x) else id) (foldr (+) a (map fst xs))) + make x ys
   
   merge e =
      case match sumView e >>= mapM powerView of
         Just es | length (nub xs) == 1 && all (`elem` [0,1]) ns  -> make (variable $ head xs) (zip as ns)
          where
            (as, xs, ns) = unzip3 es
         _ -> e

-------------------------------------------------------
-- Rewrite Rules

lineqRules :: [Rule (Context (Equation Expr))]
lineqRules = map liftRule 
   [ noDivision, merge, distribute
   , varToLeft, conToRight, scaleToOne
   ]

varToLeft :: Rule (Equation Expr)
varToLeft = makeSimpleRule "varToLeft" $ \eq -> do
   (a, x, _) <- match linearView (getRHS eq)
   when (a==0) Nothing
   return $ mergeTerms $
      if (a > 0) then minusT (fromRational a       * variable x) eq
                 else plusT  (fromRational (abs a) * variable x) eq

conToRight :: Rule (Equation Expr)
conToRight = makeSimpleRule "conToRight" $ \eq -> do
   (_, _, b) <- match linearView (getLHS eq)
   when (b==0) Nothing
   return $ mergeTerms $
      if (b > 0) then minusT (fromRational b) eq
                 else plusT  (fromRational (abs b)) eq

scaleToOne :: Rule (Equation Expr)
scaleToOne = makeSimpleRule "scaleToOne" $ \eq -> do
   (a, _, _) <- match linearView (getLHS eq)
   when (a==0 || a==1) Nothing
   return $ mergeTerms $
      divisionT (fromRational a) eq

noDivision :: Rule (Equation Expr)
noDivision = makeSimpleRule "noDivision" $ \eq@(lhs :==: rhs) -> do
   xs <- match sumView lhs
   ys <- match sumView rhs
   let list = catMaybes (map f (xs ++ ys))
       cons = catMaybes (map constantView list) 
   case (cons, list) of
      (_, [])   -> Nothing
      ([], e:_) -> return $ mergeTerms (timesT e eq)
      (ns, _)   -> return $ mergeTerms (timesT (fromInteger (foldr1 lcm ns)) eq)
 where
   f (Negate a) = f a
   f (a :/: b) = do
      guard (constantView a == Nothing)
      constantView b
      return b
   f _ = Nothing

merge :: Rule (Equation Expr)
merge = makeSimpleRule "merge" $ \e -> 
   do let new = mergeTerms e
      unless (e /= new) Nothing
      return new
      
distribute :: Rule (Equation Expr)
distribute = makeSimpleRule "distribute" $ \(lhs :==: rhs) -> 
   let f = somewhereM distributionT in
   case (f lhs,f rhs) of
      (Just new, _) -> Just (new :==: rhs)
      (_, Just new) -> Just (lhs :==: new)
      _  -> Nothing

----------------------------------------------------------------------
-- Substitution (for checking) 

substitute :: [(String, Expr)] -> Expr -> Expr
substitute sub = rec 
 where
   rec e@(Var s) = fromMaybe e (lookup s sub)
   rec e = f (map rec cs)
    where (cs, f) = uniplate e
 
solveAndCheck :: Equation Expr -> Equation Expr
solveAndCheck eq = 
   case fromContext (applyD linearEquationStrategy (inContext eq)) of
      Var x :==: e | x `notElem` collectVars e -> 
         fmap (cleanUpExpr . substitute [(x, e)]) eq
      result -> eq
 
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


---------------------------------------------------------------------
-- Linear equations in Q
{-
-- constructors: N, V, +, *, -, neg, /

genQ :: Bool -> Int -> Gen Expr
genQ b 0 = frequency [(if b then 3 else 0, return (variable "x")), (4, liftM fromInteger arbitrary)]
genQ b n = frequency [ (2, liftM2 (+) (genQ b h) (genQ b h))
                     , (2, liftM2 (-) (genQ b h) (genQ b h))
                     , (2, liftM (negate) (genQ b h))
                     , (1, liftM2 (*) (genQ b h) (genQ False h))
                     , (1, liftM2 (*) (genQ False h) (genQ b h))
                     , (2, liftM2 (/) (genQ False h) (genQ False h))
                     ]
 where h = n `div` 2

--prop1 :: Equation Expr -> Bool
--prop1 eq = solvedEquation (applyD linearEquationStrategy (inContext eq))
-- test1 = quickCheck $ forAll (liftM2 (:==:) (genQ True 10) (genQ True 10)) prop1
-}
type Q = (Rational, Rational)

(~=) :: Equation Expr -> Equation Expr -> Bool
a ~= b = toQs a == toQs b

toQs :: Equation Expr -> Maybe (Either Rational Rational)
toQs (lhs :==: rhs) = do
   (a, b) <- toQ (lhs - rhs)
   return $
      if (a == 0) then Left b
                  else Right (negate (b/a))

toQ :: Expr -> Maybe Q
toQ = foldExpr (liftM2 plus, liftM2 times, liftM2 minus, liftM neg, nat, divq, \_ -> fail "sq", \_ -> var, \_ _ -> fail "sym")
 where 
   nat n = return (0, fromIntegral n)
   var   = return (1, 0)
   plus (a, b) (c, d) = (a+c, b+d)
   times (a, b) (c, d)
      | a==0 = (b*c, b*d)
      | c==0 = (d*a, d*b)
      | otherwise = error "not linear"
   minus x y = plus x (neg y)
   neg (a, b) = (negate a, negate b)
   divq (Just (a, b)) (Just (c, d))
      | c==0 && d /=0 = return (a/d, b/d)
   divq _ _ = fail "not linear"