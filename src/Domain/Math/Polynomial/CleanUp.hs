module Domain.Math.Polynomial.CleanUp 
   ( cleanUp, cleanUpSimple
   , normalizeSum, normalizeProduct
   ) where

import Domain.Math.Data.SquareRoot
import Data.Maybe
-- import Debug.Trace
import Data.Ratio
import Data.List
import Common.View
import Domain.Math.Numeric.Views
import Domain.Math.Expr
import Domain.Math.Power.Views
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
     
sumT :: [T] -> T
sumT = head . f (const True) . f (`elem` [1,2]) . f (==1)
 where
   f p (a:b:xs)
      | p (orderT a) && p (orderT b) = 
           f p (plusT a b:xs)
      | otherwise  = a:f p (b:xs)
   f _ xs = xs

plusT :: T -> T -> T
plusT (R x) (R y) = R (x+y)
plusT (S x) (S y) = S (x+y)
plusT t@(P _ _ _) b = plusT (E $ fromT t) b 
plusT (E a) (E b) = E (a .+. b)
plusT a b = convTs plusT a b

divT :: T -> T -> T
divT (R x) (R y) = R (x/y)
divT (S x) (S y) = S (x/y)
divT t@(P _ _ _) b = divT (E $ fromT t) b 
divT (E a) (E b) = E (a ./. b)
divT a b = convTs divT a b

mulT :: T -> T -> T
mulT (R a) (R b) = R (a*b)
mulT (S a) (S b) = S (a*b)
mulT (P x1 r1 n1) (P x2 r2 n2) | x1==x2 = P x1 (r1*r2) (n1+n2)
                               | otherwise = error ""
mulT (E a) (E b) = E (a .*. b)
mulT a b = convTs mulT a b

convTs :: (T -> T -> T) -> T -> T -> T
convTs f (R a) t@(S _)       = f (S (fromRational a)) t
convTs f (R a) t@(P x _ _)   = f (P x (fromRational a) 0) t
convTs f t@(R _) e@(E _)     = f (E $ fromT t) e
convTs f t@(P _ _ _) e@(E _) = f (E $ fromT t) e
convTs f a b | orderT a > orderT b = convTs (flip f) b a
convTs _ x y = error $ "conv " ++ show (x, y)

orderT :: T -> Int
orderT (R _)     = 1
orderT (S _)     = 2
orderT (P _ _ _) = 3
orderT (E _)     = 4