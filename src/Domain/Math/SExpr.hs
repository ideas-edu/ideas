module Domain.Math.SExpr (SExpr, toExpr, simplifyExpr) where

import Common.Context
import Common.Utils (primes)
import Domain.Math.Classes
import Domain.Math.Expr
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio
import Test.QuickCheck

import Domain.LinearAlgebra.Vector

newtype SExpr = SExpr Expr deriving Eq

instance Show SExpr where
   show = show . toExpr

instance Num SExpr where
   (+) = liftS2 (+)
   (*) = liftS2 (*)
   (-) = liftS2 (-)
   negate      = liftS negate
   fromInteger = simplifyExpr . fromInteger

instance Fractional SExpr where
   (/) = liftS2 (/)
   fromRational = simplifyExpr . fromRational
   
instance Floating SExpr where
   sqrt = liftS sqrt
   pi   = simplifyExpr pi
   
instance Symbolic SExpr where
   variable = simplifyExpr . variable
   function s = simplifyExpr . function s . map toExpr
   
instance Arbitrary SExpr where
   arbitrary   = liftM simplifyExpr arbitrary
   coarbitrary = coarbitrary . toExpr
   
toExpr :: SExpr -> Expr
toExpr (SExpr e) = e

liftS  f a   = simplifyExpr $ f (toExpr a)
liftS2 f a b = simplifyExpr $ f (toExpr a) (toExpr b)

simplifyExpr :: Expr -> SExpr
simplifyExpr = SExpr . constantPropagation . simplifyMySqrt
 where
   -- bottom-up traversal
   constantPropagation e =
      let (cs, f) = uniplate e
          new     = f (map constantPropagation cs)
      in case exprToFractional new of
            Just r  -> fromRational r
            Nothing -> 
               case new of
                  -- simplify zeros
                  Con 0 :*: x -> 0
                  -- simplify square roots
                  Sqrt (Con a) -> maybe new fromInteger (hasSquareRoot a)
                  Sqrt (Con a :/: Con b) -> fromMaybe new  $ do
                     x <- hasSquareRoot a
                     y <- hasSquareRoot b
                     return $ fromInteger x / fromInteger y
                  _ -> new
            
hasSquareRoot :: Integer -> Maybe Integer
hasSquareRoot n
   | r*r == n  = Just r
   | otherwise = Nothing
 where
   r = round $ sqrt $ fromIntegral n

prop :: Expr -> Bool
prop e = f e ~= f (simplifyMySqrt e)
 where
   f = exprToFloating (\_ _ -> Nothing)
   Just a ~= Just b = abs (a-b) < 0.001
   Nothing ~= Nothing = True
   _ ~= Nothing = False

arbVector :: Gen (Vector SExpr)
arbVector = do
   a <- arbitrary
   b <- arbitrary 
   c <- arbitrary 
   return $ fromList [fromInteger a, fromInteger b, fromInteger c]

prop2 = forAll arbVector $ \v1 -> forAll arbVector $ \v2 -> 
   norm v1 /= 0 ==> let v1n = toUnit v1 in 
      isUnit v1n &&
      orthogonal v1n (makeOrthogonal v1n v2) &&
      isUnit (toUnit (makeOrthogonal v1n v2))

testje = toUnit (makeOrthogonal (toUnit v1) v2)

v1, v2, v1n, v0,v4 :: Vector SExpr
v1 = fromList [sqrt 2,sqrt 3, sqrt 4]
v2 = fromList [4,3,2]
v1n = toUnit v1
v0 = fromList [0,0,0]
v4 = fromList [1,-1,-1]

-- Simplifications needed for Gram-Schmidt
{- simplifyGS :: Expr -> Expr
simplifyGS e = 
   case f (map simplifyGS cs) of
      (a :*: b) :*: (c :*: d) | a==c && b==d -> 
         simplifyGS $ (a :*: c) :*: (b :*: d)
      (a :/: b) :*: (c :/: d) | a==c && b==d ->
         simplifyGS $ (a :*: c) :/: (b :*: d)
      Sqrt a :*: Sqrt b | a==b ->
         a
      new -> new
 where
   (cs, f) = uniplate e -}
   
e0, e1, e2, e3, e4, e5 :: SExpr
e0 = innerProduct (v2 - scale (innerProduct v2 v1n) v1n) v1n
e1 = innerProduct v2 v1n - innerProduct (scale (innerProduct v2 v1n) v1n) v1n
e2 = innerProduct v2 v1n - (innerProduct v2 v1n) * (innerProduct v1n v1n)
e3 = innerProduct v2 v1n - (innerProduct v2 v1n) * 1
e4 = innerProduct v2 v1n - (innerProduct v2 v1n)
e5 = 0
   
{- commonSubexpression :: Expr -> Expr -> [Expr]
commonSubexpression a b =
   [ x | x <- universe a, isNothing (exprToFractional x), y <- universe b, x==y ]
   
findCSE :: Expr -> [Expr]
findCSE e = rec cs ++ concatMap findCSE cs
 where 
   (cs, _) = uniplate e
   rec []     = []
   rec (x:xs) = concatMap (commonSubexpression x) xs ++ rec xs -}
   
data MySqrt = C Rational | S Rational Integer | P MySqrt MySqrt deriving Show

simplifyMySqrt :: Expr -> Expr
simplifyMySqrt e = maybe e (fromMySqrt . mergeMySqrt) (toMySqrt e)

toMySqrt :: Monad m => Expr -> m MySqrt
toMySqrt = foldExpr (bin (!+), bin (!*), bin (!-), unop neg, con, bin (!/), unop msqrt, err, const err)
 where
   err _ = fail "toMySqrt"
   bin  f a b = join (liftM2 f a b)
   unop f a = join (liftM f a)
   con  = return . C . fromIntegral
   
   -- plus
   C 0 !+ x = return x 
   x !+ C 0 = return x 
   C a !+ C b = return $ C (a+b) 
   S a n !+ S b m | n==m = return $ S (a+b) n 
   a !+ b = return $ P a b
   -- times
   C 1 !* x = return x 
   x !* C 1 = return x 
   (P a b) !* x = bin (!+) (a !* x) (b !* x)
   x !* (P a b) = bin (!+) (x !* a) (x !* b)
   C a !* C b  = return $ C (a * b) 
   C a !* S n b = return $ S (a*n) b 
   S n b !* C a = return $ S (a*n) b 
   S n a !* S m b = bin (!*) (return (C (n*m))) (return $ squareRoot (a*b))
   _ !* _ = err ""
   -- negate
   neg (C a) = return $ C (negate a) 
   neg (S r n) = return $ S (negate r) n 
   neg (P a b)  = bin (!+) (neg a) (neg b)
   -- minus
   a !- b = bin (!+) (return a) (neg b)
   -- division
   a !/ b = bin (!*) (return a) (mrecip b)
   -- recipient
   mrecip (C a)    = return $ C (recip a) 
   mrecip (S a n) = return $ S (recip (fromIntegral n*a)) n 
   mrecip (P a b)  = err ()
   -- square root
   msqrt (C r) 
      | denominator r == 1 = 
           return $ squareRoot (numerator r) 
      | otherwise =  
           bin (!/) (msqrt $ C $ fromIntegral $ numerator r) (msqrt $ C $ fromIntegral $ denominator r)
   msqrt _ = err()

squareRoot :: Integer -> MySqrt  
squareRoot n =
   case hasSquareRoot n of
      Just i  -> C (fromInteger i)
      Nothing -> rec 1 n (map fromIntegral primes)
 where
   rec c n (p:ps)
      | n `mod` (p*p) == 0 = rec (c*p) (n `div` (p*p)) (p:ps)
      | p*p > n = S (fromInteger c) n
      | otherwise = rec c n ps
   
mergeMySqrt :: MySqrt -> MySqrt
mergeMySqrt = merge . sortBy cmp . collect
 where
   collect (P a b) = collect a ++ collect b
   collect e = [e]
   
   cmp (S _ a) (S _ b) = compare a b
   cmp (C _) _ = LT
   cmp _ (C _) = GT
   
   merge (C a:C b:rest) = merge (C (a+b) : rest)
   merge (S a b:S c d:rest) | b==d = merge (S (a+c) b:rest)
   merge [e] = e
   merge (e:es) = P e (merge es)

fromMySqrt :: MySqrt -> Expr
fromMySqrt (C r)   = fromRational r
fromMySqrt (S r n) = fromRational r * Sqrt (fromInteger n)
fromMySqrt (P a b) = fromMySqrt a + fromMySqrt b