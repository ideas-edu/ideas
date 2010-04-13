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
module Domain.Math.Polynomial.CleanUp 
   ( cleanUp, cleanUpRelation, cleanUpExpr, cleanUpExpr2
   , cleanUpSimple, collectLikeTerms
   , normalizeSum, normalizeProduct
   ) where

import Common.Uniplate
import Common.View
import Control.Monad
import Data.List
import Data.Maybe
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Data.SquareRoot
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Power.Views
import Domain.Math.Simplification (smartConstructors)
import Domain.Math.SquareRoot.Views
import Prelude hiding ((^), recip)
import qualified Domain.Math.Data.SquareRoot as SQ
import qualified Prelude
import Data.Ratio

----------------------------------------------------------------------
-- Root simplification

simplerRoot :: Rational -> Integer -> Expr
simplerRoot a b 
   | b < 0          = 1 ./. simplerRoot a (abs b)
   | a < 0 && odd b = neg (simplerRoot (abs a) b)
   | otherwise      = f (numerator a) b ./. f (denominator a) b
 where
   f x y
      | x == 0              = 0
      | y == 0 || x <= 0    = root (fromIntegral x) (fromIntegral y)
      | a Prelude.^ y == x  = fromIntegral a
      | otherwise           = root (fromIntegral x) (fromIntegral y)
    where
      a = round (fromIntegral x ** (1 / fromIntegral y))

----------------------------------------------------------------------
-- Expr normalization

collectLikeTerms :: Expr -> Expr
collectLikeTerms = simplifyWith f sumView
 where
   f = normalizeSum . map (simplifyWith (second normalizeProduct) productView)

normalizeProduct :: [Expr] -> [Expr]
normalizeProduct ys = f [ (match rationalView y, y) | y <- ys ]
 where  
   f []                    = []
   f ((Nothing  , e):xs)   = e:f xs
   f ((Just r   , _):xs)   = 
      let cs   = r : [ c | (Just c, _) <- xs ]
          rest = [ x | (Nothing, x) <- xs ]
      in build rationalView (product cs):rest

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
      rs  = r:map fst (mapMaybe fst js)
      new | null js   = e
          | otherwise = build rationalView (sum rs) .*. a 

------------------------------------------------------------
-- Testing

{-
-- List with hard cases
hardCases = map cleanUpExpr $ let x=Var "x" in
  [ -1/2*x*(x/1)
  , (x/(-3))
  , (x/(-3))^2
  , (0-x)*(-x)/(-5/2)
  , (x/(-1))^2
  , (x/(-1))^2-(-7/2)*x/(-1)
  , (x^2+0)*3
  , -(49/9*x^2+0^2)*(3/16)
  , (0*x-(-x^2))*(-3)
  , x^2 - x^2
  , x^2-x^2-(x+x)*1
  , x^2/(16/3)-x^2*(-1/3)-(x+(-26/3)-x^2)*1
  , (-7+7*x)^2-(x*0)^2/(-3)
  , 1*(x+93)+4
  , (1*(x+(-93/5))-(-4+x/19))/8-(x^2-x+(19/2-x)-34/3*(x*(-41/2)))/9
  ] -}
          
------------------------------------------------------------
-- Cleaning up

cleanUpSimple :: Expr -> Expr
cleanUpSimple = transform (f4 . f2 . f1)
 where
   use v = simplifyWith (assoPlus v) sumView
   f1    = simplify rationalView
   f2    = use identity
   f4    = smartConstructors

cleanUpRelation :: OrList (Relation Expr) -> OrList (Relation Expr)
cleanUpRelation = simplifyWith cleanUp (switchView equationView)

cleanUp :: OrList (Equation Expr) -> OrList (Equation Expr)
cleanUp = idempotent . join . fmap (keepEquation . fmap cleanUpExpr)

keepEquation :: Equation Expr -> OrList (Equation Expr)
keepEquation eq@(a :==: b)
   | any falsity (universe a ++ universe b) = false
   | a == b    = true
   | otherwise = 
        case (match rationalView a, match rationalView b) of
           (Just r, Just s) 
              | r == s    -> true
              | otherwise -> false
           _              -> return eq
 where
   falsity (Sqrt e)  = maybe False (<0)  (match rationalView e)
   falsity (_ :/: e) = maybe False (==0) (match rationalView e)
   falsity _         = False

-- also simplify square roots
cleanUpExpr2 :: Expr -> Expr
cleanUpExpr2 = cleanUpExpr . transform (simplify (squareRootViewWith rationalView))

cleanUpExpr :: Expr -> Expr
cleanUpExpr = cleanUpBU2 {- e = if a1==a2 && a2==a3 && a3==a3 && a3==a4 then a1 else error $ "\n\n\n" ++ unlines (map show
   [e, a1, a2, a3, a4])
 where
   a1 = cleanUpFix e
   a2 = cleanUpBU e
   a3 = cleanUpBU2 e
   a4 = cleanUpLattice e -}
      
------------------------------------------------------------
-- Technique 1: fixed points of views
{-
cleanUpFix :: Expr -> Expr
cleanUpFix = fixpoint (f4 . f3 . f2 . f1)
 where
   use v = transform (simplifyWith (assoPlus v) sumView)
 
   f1 = use rationalView
   f2 = use (squareRootViewWith rationalView)
   f3 = use (powerFactorViewWith rationalView)
   f4 = smartConstructors
-}
assoPlus :: View Expr a -> [Expr] -> [Expr]
assoPlus v = rec . map (simplify v)
 where
   rec (x:y:zs) =
      case canonical v (x+y) of
         Just a  -> assoPlus v (a:zs)
         Nothing -> x:assoPlus v (y:zs)
   rec xs = xs

------------------------------------------------------------
-- Technique 2a: one bottom-up traversal
{-
cleanUpBU :: Expr -> Expr
cleanUpBU = transform (f4 . f3 . f2 . f1)
 where
   use v = simplifyWith (assoPlus v) sumView
 
   f1 = simplify rationalView
   f2 = simplify (squareRootViewWith rationalView)
   f3 = use (powerFactorViewWith rationalView)
   f4 = smartConstructors
-}
------------------------------------------------------------
-- Technique 2b: one bottom-up traversal

cleanUpBU2 :: Expr -> Expr
cleanUpBU2 = transform $ \e -> 
   case ( canonical rationalView e
        , canonical specialSqrtOrder e
        , match sumView e
        ) of
      (Just a, _, _) -> a
      (_, Just a, _) -> -- Just simplify order of terms with square roots for now
                        transform smart a
      (_, _, Just xs) | length xs > 1 -> 
         build sumView (assoPlus (powerFactorViewWith rationalView) xs)
      _ -> case canonical (powerFactorViewWith rationalView) e of
              Just a  -> a
              Nothing -> smart e

specialSqrtOrder :: View Expr [Expr]
specialSqrtOrder = sumView >>> makeView f id
 where
   make = match (squareRootViewWith rationalView)
   cmp (_, x) (_, y) = g x `compare` g y
   g = isNothing . fromSquareRoot
   f xs = do
      ys <- mapM make xs
      return $ map fst $ sortBy cmp $ zip xs ys

smart :: Expr -> Expr
smart (a :*: b) = a .*. b
smart (a :/: b) = a ./. b
smart expr@(Sym s [x, y]) 
   | s == powerSymbol = x .^. y
   | s == rootSymbol  = fromMaybe expr $ 
        liftM2 simplerRoot (match rationalView x) (match integerView y)
smart (Negate a) = neg a
smart (a :+: b) = a .+. b
smart (a :-: b) = a .-. b
smart (Sqrt (Nat n)) = simplerRoot (fromIntegral n) 2
smart e = e

------------------------------------------------------------
-- Technique 3: lattice of views
{-
data T = R Rational 
       | S (SquareRoot Rational)
       | P String Rational Int
       | E Expr deriving Show
   
cleanUpLattice :: Expr -> Expr
cleanUpLattice = fromT . toT

fromT :: T -> Expr
fromT (R r)     = fromRational r
fromT (S s)     = build (squareRootViewWith rationalView) s
fromT (P x r n) = build (powerFactorViewForWith x rationalView) (r, n)
fromT (E e)     = e

toT :: Expr -> T
toT (Nat n) = R (fromInteger n)
toT (x :/: y) = divT (toT x) (toT y)
toT (x :*: y) = mulT (toT x) (toT y)
toT (Var x) = P x 1 1
toT (Sym s [x, y]) | s == powerSymbol =
   case (toT x, toT y) of
      (R x, R y) | denominator y == 1  ->
         R (x Prelude.^ fromInteger (numerator y))
      (P x a n, R y) | denominator y == 1 -> 
         P x (a Prelude.^ numerator y) (n*fromInteger (numerator y))
      (x, y) -> E (fromT x .^. fromT y)
toT e@(Sqrt _) = fromMaybe (E e) $ do -- Also here, too simplistic
   s <- match (squareRootViewWith rationalView) e
   return (S s)
toT (Negate e) = negT (toT e)
toT expr =
   case match sumView expr of
      Just xs | length xs > 1 -> sumT (map toT xs)
      _ -> error $ show expr
      
negT :: T -> T
negT (R r)     = R (negate r)
negT (S s)     = S (negate s)
negT (P x r n) = P x (negate r) n
negT (E e)     = E (neg e)
     
sumT :: [T] -> T
sumT = head . f (const True) . f (`elem` [1,2]) . f (==1) . concatMap g
 where
   g e@(E a) = case match sumView a of
                  Just xs | length xs > 1 -> map (upgr . E) xs
                  _ -> [e]
   g a = [a]
 
   f p (a:b:xs)
      | p (orderT a) && p (orderT b) = 
           f p (plusT a b:xs)
      | otherwise  = a:f p (b:xs)
   f _ xs = xs

plusT :: T -> T -> T
plusT (R 0) t = t -- ?????
plusT t (R 0) = t -- ?????
plusT (R x) (R y) = R (x+y)
plusT (S x) (S y) = S (x+y)
plusT t@(P _ _ _) b = plusT (E $ fromT t) b 
plusT (E a) (E b) = E (a .+. b)
plusT a b = convTs plusT a b

divT :: T -> T -> T
divT t (R 1) = t -- ?????
divT t (R (-1)) = negT t -- ?????
divT (R x) (R y) | y /= 0 = R (x/y)
divT t@(R _) b@(R _) = divT (E $ fromT t) b
divT (S x) (S y) = S (x/y)
divT t@(P _ _ _) b = divT (E $ fromT t) b 
divT (E a) (E b) = E (a ./. b)
divT a b = convTs divT a b

mulT :: T -> T -> T
mulT (R 0) _     = R 0 -- ?????
mulT _ (R 0)     = R 0 -- ?????
mulT t (R 1)     = t -- ????
mulT (R 1) t     = t -- ?????
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

upgr :: T -> T
upgr (E e) =
   case (match (squareRootViewWith rationalView) e, match (powerFactorViewWith rationalView) e) of
      (Just a, _) -> upgr (S a)
      (_, Just (x, a, n)) -> upgr (P x a n)
      _ -> E e
upgr (S a) = maybe (S a) R (fromSquareRoot a)
upgr (P _ a n) | n==0 = R a
upgr t = t -}