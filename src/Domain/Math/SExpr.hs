module Domain.Math.SExpr (SExpr, toExpr, simplifyExpr, simplify, hasSquareRoot) where

import Common.Utils (safeHead)
import Common.Context
import Domain.Math.Classes
import Domain.Math.Expr
import Domain.Math.Constrained
import Domain.Math.Rules
import Control.Monad
import Data.Maybe
import Test.QuickCheck

newtype SExpr = SExpr (Constrained Expr)

instance Show SExpr where
   show = show . toExpr -- !!

instance Eq SExpr where
   x == y = toExpr x == toExpr y -- !!

instance Num SExpr where
   (+) = liftS2 (+)
   (*) = liftS2 (*)
   (-) = liftS2 (-)
   negate      = liftS negate
   fromInteger = make . fromInteger

instance Fractional SExpr where
   (/) = liftS2 (/)
   fromRational = make . fromRational
   
instance Floating SExpr where
   sqrt = liftS sqrt
   pi   = make pi
   
instance Symbolic SExpr where
   variable   = make . variable
   function s = liftSs (function s)
   
instance Arbitrary SExpr where
   arbitrary   = liftM (make . toConstrained) arbitrary -- !!
   coarbitrary = coarbitrary . toExpr -- !!
   
toExpr :: SExpr -> Expr
toExpr (SExpr e) = fromConstrained e

simplifyExpr :: Expr -> SExpr
simplifyExpr = make . toConstrained

liftS  f (SExpr a)           = make $ f a
liftS2 f (SExpr a) (SExpr b) = make $ f a b
liftSs f xs = make $ f [ e | SExpr e <- xs ]

make :: Constrained Expr -> SExpr
make = simplify . SExpr

-----------------------------------------------------------------------
-- Simplifications

simplify :: SExpr -> SExpr
simplify (SExpr a) = SExpr (liftC simplifyExpr2 a)

simplifyExpr2 :: Expr -> Expr
simplifyExpr2 = fixpoint (transformBU f) 
 where 
   f = applyRules . constantPropagation
            
constantPropagation :: Expr -> Expr
constantPropagation e =
   maybe e fromRational (exprToFractional e)

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
 
applyRules :: Expr -> Expr
applyRules e = 
   fromMaybe e $ safeHead [ a | r <- rs, a <- match r e ]
 where
   rs = [ rule2 "Def. minus" $ \x y -> x-y ~> x+(-y)
        , ruleZeroPlus, ruleZeroPlusComm 
        , ruleZeroTimes, ruleZeroTimesComm, ruleOneTimes, ruleOneTimesComm
        , ruleInvNeg, ruleZeroNeg
        , ruleZeroDiv, ruleOneDiv
        , ruleSimplPlusNeg, ruleSimplPlusNegComm
        , ruleSimplDiv, ruleSimpleSqrtTimes
        , rule2 "Temp1" $ \x y -> x * (1/y) ~> x/y
--        , rule2 "Temp2" $ \x y -> (1/y) * x ~> x/y
        , rule3 "Temp3" $ \x y z -> (x/z) * (y/z) ~> (x*y)/(z*z)
        , rule3 "Temp4" $ \x y z -> (x/z) + (y/z) ~> (x+y)/z
        , rule2 "Temp5" $ \x y -> (x/y)/y ~> x/(y*y)
        
        -- , rule5 "Xtreme" $ \a b x y c -> a*(x+ negate (a*c)) + b*(y+negate (b*c)) ~> ((a*x)+(b*y))-((a*a+b*b)*c)
        
--        , rule5 "Xtreme" $ \a b c d e -> (a + (-(b*c)))+(d + (- (e*c))) ~> (a+d)-((b+e)*c)
--        , rule3 "TempD" $ \x y z -> x*(y + (-z)) ~> (x*y) - (x*z)
        
--        , rule3 "Temp5" $ \x y z -> (x/y)/z ~> x/(y*z)
--        , rule3 "Temp6" $ \x y z -> x/(y/z) ~> (x*z)/y
--        , rule2 "Temp7" $ \x y -> sqrt (x/y) ~> sqrt x / sqrt y
        ]
 
{-
special :: Expr -> Expr
special e0 = fromMaybe e0 $ do 
   triples <- mapM f (collectPlus e0)
   guard (check triples)
   return $ partOne triples - (partTwo triples * partThree triples)
 where
   f (a1 :*: (x :+: Negate (a2 :*: c))) | a1==a2 = Just (a1, x, c)
   f _ = Nothing
   check (x:xs) = all ((==thd3 x) . thd3) xs
   thd3 (_, _, a) = a
   
   partOne   = foldr1 (+) . map (\(a,x,_) -> a*x)
   partTwo   = foldr1 (+) . map (\(a,_,_) -> a*a)
   partThree = thd3 . head -}
   
transformBU :: Uniplate a => (a -> a) -> a -> a
transformBU g a = g $ f $ map (transformBU g) cs
 where
   (cs, f) = uniplate a
   
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = stop . iterate f 
 where
   stop (x:xs) 
      | x == head xs = x
      | otherwise    = stop xs