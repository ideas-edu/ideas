module Domain.Math.Polynomial.CleanUp 
   ( cleanUp, cleanUpSimple
   , normalizeSum, normalizeProduct
   ) where

import Domain.Math.Data.SquareRoot
import Data.Maybe
-- import Debug.Trace
import Data.Ratio
import Data.List
import Domain.Math.Expr
import Domain.Math.View.Basic
import Domain.Math.View.Power
import Domain.Math.SquareRoot.Views
import Common.Uniplate
import Domain.Math.Simplification (smartConstructors)
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList

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
          
------------------------------------------------------------
-- Cleaning up

cleanUpSimple :: Equation Expr -> Equation Expr
cleanUpSimple = fmap (smartConstructors . transform (simplify rationalView))

cleanUp :: OrList (Equation Expr) -> OrList (Equation Expr)
cleanUp (OrList xs) = OrList (filter keepEquation (map (fmap cleanUpExpr) xs))

keepEquation :: Equation Expr -> Bool
keepEquation (a :==: b) = Prelude.not (trivial || any falsity (universe a ++ universe b))  
 where
   trivial = noVars a && noVars b
   falsity (Sqrt e)  = maybe False (<0)  (match rationalView e)
   falsity (_ :/: e) = maybe False (==0) (match rationalView e)
   falsity _         = False

cleanUpExpr :: Expr -> Expr
cleanUpExpr e = a2 -- if a1==a2 then a1 else error $ show (e, a1, a2)
 where
   a1 = ff e
   a2 = cleanUp2 e
   ff = smartConstructors . f2 . f1 . smartConstructors . simplifyWith sumList sumView
 
   f1 = transform (simplify powerFactorView)
   f2 = transform (simplify (squareRootViewWith rationalView))
   
   sumList = rec . map (\a -> maybe (Left a) Right $ match rationalView a) 
    where
      rec (Right r1:Right r2:rest) = rec (Right (r1+r2):rest)
      rec (Right r:xs)   = fromRational r:rec xs
      rec (Left a:xs) = a:rec xs
      rec [] = []
      
data T = R Rational 
       | S (SquareRoot Rational)
       | P String Rational Int
       | E Expr deriving Show
   
cleanUp2 :: Expr -> Expr
cleanUp2 e = {-trace (show e) -} ((fromT . toT) e)

fromT :: T -> Expr
fromT (R r) = fromRational r
fromT (S s) = build (squareRootViewWith rationalView) s
fromT (P x r n) = build (powerFactorViewForWith x rationalView) (r, n)
fromT (E e) = e

toT :: Expr -> T
toT (Nat n) = R (fromInteger n)
toT (x :/: y) = divT (toT x) (toT y)
toT (x :*: y) = mulT (toT x) (toT y)
toT (Var x) = P x 1 1
toT e@(Sym s [_, _]) | s == powerSymbol = fromMaybe (E e) $ do -- Actually, also take smart constr ^ into ac
   s <- selectVar e
   (r, n) <- match (powerFactorViewForWith s (rationalView)) e
   return (P s r n)
toT e@(Sqrt _) = fromMaybe (E e) $ do -- Also here, too simplistic
   s <- match (squareRootViewWith rationalView) e
   return (S s)
toT (Negate e) = negT (toT e)
toT expr =
   case match sumView expr of
      Just xs | length xs > 1 -> sumT (map toT xs)
      _ -> error $ show expr
      
negT :: T -> T
negT (R r) = R (negate r)
negT (S s) = S (negate s)
negT (P x r n) = P x (negate r) n
negT (E e) = E (neg e)

{- recipT :: T -> T
recipT (R r) = R (Prelude.recip r)
recipT (S s) = S (Prelude.recip s)
recipT t     = E (Domain.Math.View.Basic.recip (fromT t)) -}
     
sumT :: [T] -> T
sumT = f3 . f2 . f1
 where
   f1 (R a:R b:xs) = f1 (R (a+b):xs)
   f1 (x:xs)       = x:f1 xs
   f1 []           = []
   
   f2 (S a:S b:xs) = f2 (S (a+b):xs)
   f2 (R a:S b:xs) = f2 (S (fromRational a+b):xs)
   f2 (S a:R b:xs) = f2 (S (a+fromRational b):xs)
   f2 (x:xs)       = x:f2 xs
   f2 []           = []
   
   f3 [t] = t
   f3 ts  = E (build sumView (map fromT ts))

divT :: T -> T -> T
divT (R x) (R y) = R (x/y)
divT (E x) (R y) = E (x ./. fromRational y)
divT (S s) (R y) = S (s/fromRational y)
divT (P x r n) (R y) = P x (r/y) n
divT x y = error ("divT: " ++ show (x, y))

mulT :: T -> T -> T
mulT (R a) (R b) = R (a*b)
mulT (R a) (S s) = S (fromRational a*s)
mulT (R a) (P x r n) = P x (r*a) n
mulT (P x r n) (R a) = P x (r*a) n
mulT (R a) (E e) = E (fromRational a .*. e)
mulT (E e) (R a)  = E (fromRational a .*. e)
mulT (P x1 r1 n1) (P x2 r2 n2) |x1==x2 = P x1 (r1*r2) (n1+n2)
mulT a@(P _ _ _) (E e) = E (fromT a .*. e)
mulT (E e) a@(P _ _ _) = E (fromT a .*. e)
mulT (E a) (E b) = E (a .*. b)
mulT x y = error ("mulT: " ++ show (x, y))