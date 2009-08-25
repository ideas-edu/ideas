module Domain.Math.Simplification 
   ( Simplify(..), smartConstructors
   , Simplified, simplified, liftS, liftS2
   ) where

import Common.Context
import Common.Uniplate
import Data.List
import Data.Maybe
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.View.Basic hiding (simplify, recip)
import Domain.Math.Data.Equation

class Simplify a where
   simplify :: a -> a

instance Simplify a => Simplify (Context a) where
   simplify = fmap simplify

instance Simplify a => Simplify (Equation a) where
   simplify = fmap simplify

instance Simplify a => Simplify [a] where
   simplify = fmap simplify

instance Simplify Expr where
   simplify = smartConstructors . mergeAlike . distribution . constantFolding

data Simplified a = S a

instance Eq a => Eq (Simplified a) where
   S x == S y = x==y

instance Show a => Show (Simplified a) where
   show (S x) = show x

instance (Num a, Simplify a) => Num (Simplified a) where
   (+)         = liftS2 (+)
   (*)         = liftS2 (*)
   (-)         = liftS2 (-)
   negate      = liftS negate
   abs         = liftS abs
   signum      = liftS signum
   fromInteger = simplified . fromInteger

instance (Fractional a, Simplify a) => Fractional (Simplified a) where
   (/)          = liftS2 (/)
   recip        = liftS recip
   fromRational = simplified . fromRational

instance Simplify (Simplified a) where
   simplify = id

simplified :: Simplify a => a -> Simplified a
simplified = S . simplify

liftS :: Simplify a => (a -> a) -> Simplified a -> Simplified a
liftS f (S x) = simplified (f x)

liftS2 :: Simplify a => (a -> a -> a) -> Simplified a -> Simplified a -> Simplified a
liftS2 f (S x) (S y) = simplified (f x y)

------------------------------------------------------------
-- Simplification with the smart constructors

smartConstructors :: Expr -> Expr
smartConstructors = transform $ \expr ->
   case expr of
      a :+: b  -> a .+. b
      a :-: b  -> a .-. b
      Negate a -> neg a
      a :*: b  -> a .*. b
      a :/: b  -> a ./. b
      Sym s [a, b] | s == powerSymbol -> 
         a .^. b
      _        -> expr

-------------------------------------------------------------
-- Distribution of constants

distribution :: Expr -> Expr
distribution = transformTD $ \expr ->
   fromMaybe expr $ do
   case expr of
      a :*: b -> do
         (x, y) <- match plusView a
         r      <- match rationalView b
         return $ (fromRational r .*. x) .+. (fromRational r .*. y)
       `mplus` do
         r      <- match rationalView a
         (x, y) <- match plusView b
         return $ (fromRational r .*. x) .+. (fromRational r .*. y)
      a :/: b -> do
         xs <- match sumView a
         guard (length xs > 1)
         return $ build sumView $ map (./. b) xs
      _ -> Nothing
      
-------------------------------------------------------------
-- Constant folding

-- Not an efficient implementation: could be improved if necessary
constantFolding :: Expr -> Expr
constantFolding expr = 
   case match rationalView expr of
      Just r  -> fromRational r
      Nothing -> let (xs, f) = uniplate expr
                 in f (map constantFolding xs)
                 
----------------------------------------------------------------------
-- merge alike for sums and products
   
mergeAlike :: Expr -> Expr
mergeAlike a =
   case (match sumView a, match productView a) of
      (Just xs, _) | length xs > 1 -> 
         build sumView (sort $ mergeAlikeSum $ map mergeAlike xs)
      (_, Just (b, ys)) | length (filter (/= 1) ys) > 1 -> 
         build productView (b, sort $ mergeAlikeProduct $ map mergeAlike ys)
      _ -> a

mergeAlikeProduct :: [Expr] -> [Expr]
mergeAlikeProduct ys = f [ (match rationalView y, y) | y <- ys ]
  where  f []                    = []
         f ((Nothing  , e):xs)   = e:f xs
         f ((Just r   , _):xs)   = 
           let  cs    = r :  [ c  | (Just c   , _)  <- xs ]
                rest  =      [ x  | (Nothing  , x)  <- xs ]
           in   build rationalView (product cs):rest

mergeAlikeSum :: [Expr] -> [Expr]
mergeAlikeSum xs = rec [ (Just $ pm 1 x, x) | x <- xs ]
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