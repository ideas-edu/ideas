module Domain.Math.SExpr (SExpr, toExpr, simplifyExpr, simplify, hasSquareRoot) where

import Common.Utils
import Common.Uniplate
import Domain.Math.Classes
import Domain.Math.Expr
import Domain.Math.Constrained
import Domain.Math.Rules
import Domain.Math.Rewriting
import Control.Monad
import Data.List
import Data.Ratio
import Data.Maybe
import Data.Monoid
import Test.QuickCheck

newtype SExpr = SExpr (Constrained (Con Expr) Expr)

instance Show SExpr where
   show = show . toExpr -- !!

instance Eq SExpr where
   x == y =  let nonsense (SExpr e) = contradiction (proposition e) in
             equalACs exprACs (toExpr x) (toExpr y)
          || nonsense x
          || nonsense y

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
make = simplify . SExpr

-----------------------------------------------------------------------
-- Simplifications

simplify :: SExpr -> SExpr
simplify (SExpr c) = SExpr $ g $ liftM rewriteGS c >>= fixpointM (transformM f)
 where
   f a = (return . constantPropagation) a >>= applyRules >>= (return . simplifySquareRoots)
   g (C c a) = C (simplifyPropCon c) a

-- special care is taken for associative and commutative operators       
constantPropagation :: Expr -> Expr
constantPropagation e =
   case findOperatorAC exprACs e of
      Just ac
         | not (null xs) && not (null ys) -> 
              let new = constantPropagation (buildAC ac (catMaybes $ map snd xs))
              in buildAC ac (new:map fst ys)
       where 
         (xs, ys) = partition (isJust . snd) $ map f $ collectAC ac e
         f a = (a, exprToFractional a)
      _ -> maybe e fromRational (exprToFractional e)

simplifySquareRoots :: Expr -> Expr
simplifySquareRoots e =
   case e of
      Sqrt (Con a) -> maybe e fromInteger (hasSquareRoot a)
      _ -> e

hasSquareRoot :: Integer -> Maybe Integer
hasSquareRoot n
   | r*r == n  = Just r
   | otherwise = Nothing
 where
   r = round $ sqrt $ fromIntegral n

applyRules :: Expr -> Constrained (Con Expr) Expr
applyRules e = 
   fromMaybe (return e) $ safeHead [ constrain p >> return a | r <- rs, (a, p) <- matchM r e ]
 where
   rs = [ rule2 "Def. minus" $ \x y -> x-y ~> x+(-y)
        , ruleZeroPlus, ruleZeroPlusComm 
        , ruleZeroTimes, ruleZeroTimesComm, ruleOneTimes, ruleOneTimesComm
        , ruleInvNeg, ruleZeroNeg
        , ruleZeroDiv, ruleOneDiv
        , ruleSimplPlusNeg, ruleSimplPlusNegComm
        , ruleSimplDiv, ruleSimpleSqrtTimes
        , rule2 "Temp1" $ \x y -> x * (1/y) ~> x/y
        , rule3 "Temp3" $ \x y z -> (x/z) * (y/z) ~> (x*y)/(z*z)
        , rule3 "Temp4" $ \x y z -> (x/z) + (y/z) ~> (x+y)/z
        , rule2 "Temp5" $ \x y -> (x/y)/y ~> x/(y*y)
        ]

-- Gram-Schmidt view
data ViewGS = PlusGS ViewGS ViewGS | TimesGS Rational Integer

rewriteGS :: Expr -> Expr
rewriteGS e = maybe e (fromViewGS . sortAndMergeViewGS) (toViewGS e)

toViewGS :: Expr -> Maybe ViewGS
toViewGS = foldExpr (bin plus, bin times, bin min, unop neg, con, bin div, unop sqrt, err, const err)
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
         Con x  :==: Con y  -> if x==y then T else F
         Sqrt x :==: Sqrt y -> (x .== y) /\ (x .>= 0) /\ (y .>= 0)
         Con x  :==: Sqrt y
            | x >= 0    -> Con (x*x) .== y
            | otherwise -> F
         Sqrt x :==: Con y
            | y >= 0    -> x .== Con (y*y)
            | otherwise -> F
         -- less-than constraints
         Con x  :<: Con y  -> if x<y then T else F
         Sqrt x :<: Sqrt y -> (x .< y) /\ (x .>= 0)
         Con x  :<: Sqrt y
            | x >= 0    -> (Con (x*x) .< y)
            | otherwise -> y .>= 0
         Sqrt x :<: Con y
            | y >= 0    -> (x .< Con (y*y)) /\ (x .>= 0)
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