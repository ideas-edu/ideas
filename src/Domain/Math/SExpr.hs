{-# OPTIONS -XEmptyDataDecls #-}
module Domain.Math.SExpr
   ( --simpler, simplerGS, simplerLin, simplerMatrix, simplerVectors, simplerEquations
  -- , cleanUpWith, cleanUpStrategy
     lam, SExpr, toExpr
   , simplifyExpr, simplify, hasSquareRoot
   ) where

import Common.Utils
import Common.View hiding (Simplification, simplify)
import qualified Common.View as View
import Common.Uniplate (transformM, children, Uniplate(..))
import Common.Rewriting
import Domain.Math.Symbolic
import Domain.Math.Expr
import Domain.Math.Constrained
import Domain.Math.Rules
import Control.Monad
import Data.List
import Data.Ratio
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid
import Test.QuickCheck hiding (label)

{-
simpler, simplerGS, simplerLin :: View.Simplification Expr
simpler    = View.makeSimplification (fromConstrained . simplify)
simplerGS  = View.makeSimplification (fromConstrained . simplifyGS)
simplerLin = View.makeSimplification (fromConstrained . simplifyLin)

simplerMatrix :: View.Simplification (Matrix Expr)
simplerMatrix = View.makeSimplification (fmap (View.simplify simpler))

simplerVectors :: View.Simplification [Vector Expr]
simplerVectors = View.makeSimplification (map (fmap (View.simplify simplerGS)))

simplerEquations :: View.Simplification (Equations Expr)
simplerEquations = View.makeSimplification (map (fmap (View.simplify simplerLin)))

-- parser/pretty-printer are left unchanged
cleanUpWith :: View.Simplification a -> Exercise a -> Exercise a
cleanUpWith view ex = ex
   { strategy      = cleanUpStrategy (fmap (View.simplify view)) (strategy ex)
   , ruleset       = map (cleanUpRule (fmap (View.simplify view))) (ruleset ex)
   , equality      = \x y -> equality ex (View.simplify view x) (View.simplify view y)
   , finalProperty = finalProperty ex . View.simplify view
   , generator     = liftM (View.simplify view) (generator ex)
   }

cleanUpStrategy :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategy f s = mapRules (cleanUpRule f) 
   (label (strategyName s) (doAfter f idRule <*> unlabel s))

cleanUpRule :: (a -> a) -> Rule a -> Rule a
cleanUpRule f r
   | isMajorRule r = doAfter f r  
   | otherwise     = r -}

newtype SExpr = SExpr (Constrained (Con Expr) Expr)

instance Show SExpr where
   show = show . toExpr -- !!

instance Eq SExpr where
   x == y =  let nonsense (SExpr e) = contradiction (proposition e) in
             equalWith exprACs (toExpr x) (toExpr y)
          || nonsense x
          || nonsense y

instance Ord SExpr where
   x `compare` y = 
      let f a@(SExpr e) = 
             if contradiction (proposition e) 
             then Nothing
             else Just (normalizeWith exprACs (toExpr a))
      in f x `compare` f y

instance Num SExpr where
   (+) = liftS2 (+)
   (*) = liftS2 (*)
   (-) = liftS2 (-)
   negate      = liftS negate
   fromInteger = make . fromInteger
   abs         = liftS abs
   signum      = liftS signum

instance Fractional SExpr where
   (/) = liftS2 (/)
   fromRational = make . fromRational
   
instance Floating SExpr where
   pi      = make   pi
   sqrt    = liftS  sqrt
   (**)    = liftS2 (**)
   logBase = liftS2 logBase
   exp     = liftS  exp
   log     = liftS  log
   sin     = liftS  sin
   tan     = liftS  tan
   cos     = liftS  cos
   asin    = liftS  asin
   atan    = liftS  atan
   acos    = liftS  acos
   sinh    = liftS  sinh
   tanh    = liftS  tanh
   cosh    = liftS  cosh
   asinh   = liftS  asinh
   atanh   = liftS  atanh
   acosh   = liftS  acosh

instance Symbolic SExpr where
   variable   = make . variable
   function s = liftSs (function s)

instance Arbitrary SExpr where
   arbitrary   = liftM (make . return) arbitrary -- !!
   coarbitrary = coarbitrary . toExpr -- !!

toExpr :: SExpr -> Expr -- !!!
toExpr (SExpr e) = fromConstrained e

simplifyExpr :: Expr -> SExpr
simplifyExpr = make . return

liftS  f (SExpr a)           = make $ f a
liftS2 f (SExpr a) (SExpr b) = make $ f a b
liftSs f xs = make $ f [ e | SExpr e <- xs ]

make :: Constrained (Con Expr) Expr -> SExpr
make c = let result = SExpr $ simplify (fromConstrained c)
         in result

-----------------------------------------------------------------------
-- Simplifications

-- rewriteLin is used in the simplification procedure to "merge" terms with the
-- same variable
simplify :: Expr -> Constrained (Con Expr) Expr
simplify = g . fixpointM (transformM f) . rewriteLin
 where
   f a =   (return . constantPropagation) a 
       >>= (return . distribution) 
       >>= applyRules
       >>= (return . simplifySquareRoots)
   g (C c a) = C (simplifyPropCon c) a

-- in general, distribution rules are not desirable, but under the right circumstances 
-- it can help to further simplify expressions
distribution :: Expr -> Expr
distribution (x :*: (y :+: z)) | isRat x = (x*y)+(x*z)
distribution ((x :+: y) :*: z) | isRat z = (x*z)+(y*z)
distribution (x :*: (y :-: z)) | isRat x = (x*y)-(x*z)
distribution ((x :-: y) :*: z) | isRat z = (x*z)-(y*z)
-- simplification
distribution (x :/: y) | simplify x == simplify (-y) = -1
distribution ((n :*: a1) :+: (m :*: a2)) | isRat n && isRat m && a1==a2 = (n+m)*a1
distribution ((n :*: a1) :+: a2)         | isRat n && a1==a2 = (n+1)*a1
distribution (a1         :+: (m :*: a2)) | isRat m && a1==a2 = (1+m)*a1
distribution (a1 :+: a2)                 | a1==a2 = 2*a1
distribution ((n :*: a1) :-: (m :*: a2)) | isRat n && isRat m && a1==a2 = (n-m)*a1
distribution ((n :*: a1) :-: a2)         | isRat n && a1==a2 = (n-1)*a1
distribution (a1         :-: (m :*: a2)) | isRat m && a1==a2 = (1-m)*a1
distribution e = e

transformTD :: Uniplate a => (a -> Maybe a) -> a -> a
transformTD f a = 
   case f a of
      Just b  -> b
      Nothing -> g (map (transformTD f) cs)
 where
   (cs, g) = uniplate a

-- check whether the expression is a "rational" number
isRat :: Expr -> Bool
isRat (Nat _) = True
isRat (Nat _ :/: Nat _) = True
isRat _ = False

lam,varx :: SExpr
lam = variable "L"
varx = variable "x"

-- special care is taken for associative and commutative operators       
constantPropagation :: Expr -> Expr
constantPropagation e =
   case findOperator exprACs e of
      Just ac
         | not (null xs) && not (null ys) -> 
              let new = constantPropagation (buildWithOperator ac (catMaybes $ map snd xs))
              in buildWithOperator ac (new:map fst ys)
       where 
         (xs, ys) = partition (isJust . snd) $ map f $ collectWithOperator ac e
         f a = (a, exprToFractional a)
      _ -> maybe e (\r -> if r < 0 then negate (fromRational $ negate r) else fromRational r) (exprToFractional e)

simplifySquareRoots :: Expr -> Expr
simplifySquareRoots e =
   case e of
      Sqrt (Nat a) -> maybe e fromInteger (hasSquareRoot a)
      _ -> e

hasSquareRoot :: Integer -> Maybe Integer
hasSquareRoot n
   | r*r == n  = Just r
   | otherwise = Nothing
 where
   r = round $ sqrt $ fromIntegral n

applyRules :: Expr -> Constrained (Con Expr) Expr
applyRules e = 
   fromMaybe (return e) $ safeHead [ {- constrain p >> -} return a | r <- rs, a <- rewriteM r e ]
 where
   rs = [ -- Zeros and neutral elements
          ruleZeroPlus, ruleZeroPlusComm 
        , ruleZeroTimes, ruleZeroTimesComm
        , ruleOneTimes, ruleOneTimesComm
        , ruleZeroDiv, ruleOneDiv
        , ruleZeroNeg
        , rewriteRule "Min Zero Left"  $ \x -> 0-x :~> -x
        , rewriteRule "Min Zero Right" $ \x -> x-0 :~> x
        , rewriteRule "Min Intro Zero" $ \x -> x-x :~> 0
        , ruleSimplDiv
          -- negation rules
        , rewriteRule "Neg + Left"  $ \x y -> (-x)+y :~> y-x
        , rewriteRule "Neg + Right" $ \x y -> x+(-y) :~> x-y
        , rewriteRule "Neg - Left"  $ \x y -> (-x)-y :~> -(x+y)
        , rewriteRule "Neg - Right" $ \x y -> x-(-y) :~> x+y
        , rewriteRule "Neg * Left"  $ \x y -> (-x)*y :~> -(x*y)
        , rewriteRule "Neg * Right" $ \x y -> x*(-y) :~> -(x*y)
        , rewriteRule "Neg / Left"  $ \x y -> (-x)/y :~> -(x/y)
        , rewriteRule "Neg / Right" $ \x y -> x/(-y) :~> -(x/y)
        , ruleInvNeg
        , rewriteRule "Neg with min" $ \x y -> -(x-y) :~> y-x
          -- other rules
        , ruleSimpleSqrtTimes
        , rewriteRule "Times Div Left"  $ \x y z -> x*(y/z) :~> (x*y)/z
        , rewriteRule "Times Div Right" $ \x y z -> (x/y)*z :~> (x*z)/y 
          -- temporary rules
        , rewriteRule "Temp1" $ \x y -> x * (1/y) :~> x/y
        , rewriteRule "Temp2" $ \x y z -> (x/z) * (y/z) :~> (x*y)/(z*z)
        , rewriteRule "Temp3" $ \x y z -> (x/z) + (y/z) :~> (x+y)/z
        , rewriteRule "Temp3" $ \x y z -> (x/z) - (y/z) :~> (x-y)/z
        , rewriteRule "Temp5" $ \x y -> (x/y)/y :~> x/(y*y)
        ]

simplifyGS :: Expr -> Constrained (Con Expr) Expr
simplifyGS = simplify . rewriteGS

-- Gram-Schmidt view
data ViewGS = PlusGS ViewGS ViewGS | TimesGS Rational Integer

rewriteGS :: Expr -> Expr
rewriteGS = undefined -- View.simplify viewGS

viewGS :: View Expr ViewGS
viewGS = makeView toViewGS (fromViewGS . sortAndMergeViewGS)

toViewGS :: Expr -> Maybe ViewGS
toViewGS = undefined -- foldExpr (bin plus, bin times, bin min, unop neg, con, bin div, unop sqrt, err, const err)
 where
   err _ = fail "toMySqrt"
   bin  f a b = join (liftM2 f a b)
   unop f a = join (liftM f a)
   con n = return (TimesGS (fromIntegral n) 1)
   
   plus a b = return (PlusGS a b)   
   min a b  = bin plus (return a)  (neg b)
   neg a    = bin times (con (-1)) (return a)
   div a b  = bin times (return a) (recip b)
   
   times (PlusGS a b) c = bin plus (times a c) (times b c)
   times a (PlusGS b c) = bin plus (times a b) (times a c)
   times (TimesGS r1 n1) (TimesGS r2 n2) =
      case squareRoot (n1*n2) of
         Just (TimesGS r3 n3) -> return $ TimesGS (r1*r2*r3) n3
         _ -> Nothing
         
   recip (TimesGS r n) | r /= 0 && n /= 0 = return $ TimesGS (1 / (fromIntegral n*r)) n 
   recip _ = Nothing
   
   sqrt (TimesGS r 1) 
      | r2 == 1 = 
           squareRoot r1
      | otherwise =  
           bin div (unop sqrt $ con $ fromIntegral r1) (unop sqrt $ con $ fromIntegral r2)
    where (r1, r2) = (numerator r, denominator r)
   sqrt _ = Nothing
   
   squareRoot n = maybe (rec 1 n [2..20]) con (hasSquareRoot n) 
    where
      rec i n [] = return $ TimesGS (fromInteger i) n
      rec i n (x:xs)
         | n `mod` x2 == 0 = rec (i*x) (n `Prelude.div` x2) (x:xs)
         | otherwise       = rec i n xs
       where
         x2 = x*x
      
sortAndMergeViewGS :: ViewGS -> ViewGS
sortAndMergeViewGS = merge . sortBy cmp . collect
 where
   collect (PlusGS a b)  = collect a ++ collect b
   collect (TimesGS r n) = [(r, n)]
   
   merge ((r1, n1):(r2, n2):rest)
      | n1 == n2  = merge ((r1+r2, n1):rest)
      | otherwise = PlusGS (TimesGS r1 n1) (merge ((r2,n2):rest))
   merge [(r1, n1)] = TimesGS r1 n1
   merge _ = error "merge"
   
   cmp x y = snd x `compare` snd y

fromViewGS :: ViewGS -> Expr
fromViewGS (PlusGS a b)  = fromViewGS a + fromViewGS b
fromViewGS (TimesGS r n) = fromRational r * sqrt (fromIntegral n)

-- Linear expressions view
simplifyLin :: Expr -> Constrained (Con Expr) Expr
simplifyLin e = return $ maybe e (fromViewLin . simplifyViewLin) (toViewLin e)

-- invariant: coefficients are /= 0
data ViewLin = Lin (M.Map String Expr) Expr
   deriving (Show, Eq)

simplifyViewLin :: ViewLin -> ViewLin
simplifyViewLin (Lin m e) = makeLin (M.map f m) (f e)
 where f = fromConstrained . simplify
 
rewriteLin :: Expr -> Expr
rewriteLin e = maybe e fromViewLin (toViewLin e)

toViewLin :: Expr -> Maybe ViewLin
toViewLin expr =
   case expr of
      a :+: b -> do
         Lin m1 e1 <- toViewLin a
         Lin m2 e2 <- toViewLin b
         return $ makeLin (M.unionWith (+) m1 m2) (e1+e2)
      Negate a -> do
         Lin m e <- toViewLin a
         return $ Lin (M.map negate m) (negate e)
      a :-: b ->
         toViewLin (a :+: negate b)
      a :*: b 
         | isConstant a -> do
              Lin m e <- toViewLin b
              return $ Lin (M.map (a*) m) (a*e)
         | isConstant b -> do
              Lin m e <- toViewLin a
              return $ Lin (M.map (*b) m) (e*b)
      a :/: b
         | isConstant b -> do
              Lin m e <- toViewLin a
              return $ Lin (M.map (/b) m) (e/b)
      _ -> do
         (ms, e) <- toProduct expr
         return $ case ms of
            Just s  -> makeLin (M.singleton s e) 0
            Nothing -> makeLin M.empty e
 where
   toProduct :: Expr -> Maybe (Maybe String, Expr)
   toProduct expr =
      case expr of
         Var s -> return (Just s, 1)
         a :*: b 
            | isConstant a -> do
                 (ms, e) <- toProduct b
                 return (ms, a*e)
            | isConstant b -> do
                 (ms, e) <- toProduct a
                 return (ms, e*b)
            | otherwise -> 
                 Nothing
         _  | isConstant expr ->
                 return (Nothing, expr)
            | otherwise ->
                 Nothing

isConstant :: Expr -> Bool
isConstant = null . collectVars

makeLin :: M.Map String Expr -> Expr -> ViewLin
makeLin m c = Lin (M.filter (/=0) m) c

fromViewLin :: ViewLin -> Expr
fromViewLin (Lin m c) = make $
   [ if e == 1 then Var s else Var s * e | (s, e) <- M.toList m ] ++ [ c | c /= 0 ]
 where
   make [] = 0
   make xs = foldr1 (+) xs

-----------------------------------------------------------------------
-- Simplifications for constraints

simplifyPropCon :: Prop (Con Expr) -> Prop (Con Expr)
simplifyPropCon = fixpoint (mapProp simplifyCon . simplifyProp)

simplifyCon :: Con Expr -> Prop (Con Expr)
simplifyCon = convert . fmap simplifyExpr
 where
   f :: Con Expr -> Prop (Con Expr)
   f con = 
      case con of
         -- equality constraints
         Nat x  :==: Nat y  -> if x==y then T else F
         Sqrt x :==: Sqrt y -> (x .== y) /\ (x .>= 0) /\ (y .>= 0)
         Nat x  :==: Sqrt y
            | x >= 0    -> Nat (x*x) .== y
            | otherwise -> F
         Sqrt x :==: Nat y
            | y >= 0    -> x .== Nat (y*y)
            | otherwise -> F
         -- less-than constraints
         Nat x  :<: Nat y  -> if x<y then T else F
         Sqrt x :<: Sqrt y -> (x .< y) /\ (x .>= 0)
         Nat x  :<: Sqrt y
            | x >= 0    -> (Nat (x*x) .< y)
            | otherwise -> y .>= 0
         Sqrt x :<: Nat y
            | y >= 0    -> (x .< Nat (y*y)) /\ (x .>= 0)
            | otherwise -> F
         -- well-formedness constraints
         WF (x :/: y) -> wf x /\ (y ./= 0)
         WF (Sqrt x)  -> x .>= 0
         WF x         -> mconcat (map wf (children x))
         -- catch-all
         _ -> return con
 
   convert :: Con SExpr -> Prop (Con Expr)
   convert c = f (fmap toExpr c) `mplus`
               msum [ proposition e | SExpr e <- flattenCon c ]
   
   flattenCon :: Con a -> [a] -- can be done generically
   flattenCon con =
      case con of
         x :==: y -> [x,y]
         x :<:  y -> [x,y]
         WF x     -> [x]