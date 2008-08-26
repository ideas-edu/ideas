module Domain.Math.Expr where

import Data.Char
import Data.Ratio
import Test.QuickCheck
import Control.Monad
import Common.Context
import Domain.Math.Classes

-----------------------------------------------------------------------
-- Expression data type

data Expr = -- Num 
            Expr :+: Expr 
          | Expr :*: Expr 
          | Expr :-: Expr
          | Negate Expr
          | Con Integer
            -- Fractional & Floating
          | Expr :/: Expr   -- NaN if rhs is zero
          | Sqrt Expr       -- NaN if expr is negative
            -- Symbolic
          | Var String
          | Sym String [Expr]
   deriving (Eq, Ord)

-----------------------------------------------------------------------
-- Numeric instances (and symbolic)

instance Num Expr where
   (+) = (:+:) 
   (*) = (:*:)
   (-) = (:-:)
   negate      = Negate 
   fromInteger = Con

instance Fractional Expr where
   (/) = (:/:)
   fromRational r
      | denominator r == 1 = Con (numerator r)
      | otherwise = Con (numerator r) :/: Con (denominator r)

instance Floating Expr where
   sqrt = Sqrt
   pi   = symbol "pi"
   
instance Symbolic Expr where
   variable = Var
   function = Sym
   
-----------------------------------------------------------------------
-- Uniplate instance

instance Uniplate Expr where 
   uniplate expr =
      case expr of
         a :+: b  -> ([a,b], \[x,y] -> x :+: y)
         a :*: b  -> ([a,b], \[x,y] -> x :*: y)
         a :-: b  -> ([a,b], \[x,y] -> x :-: y)
         Negate a -> ([a]  , \[x]   -> Negate x)
         Con n    -> ([]   , \[]    -> expr)
         a :/: b  -> ([a,b], \[x,y] -> x :/: y)
         Sqrt a   -> ([a]  , \[x]   -> Sqrt x)
         Var s    -> ([]   , \[]    -> expr)
         Sym f xs -> ([]   , \[]    -> expr)

-----------------------------------------------------------------------
-- Arbitrary instance

instance Arbitrary Expr where
   arbitrary = sized arbExpr
   coarbitrary expr =
      case expr of 
         a :+: b  -> variant 0 . coarbitrary a . coarbitrary b
         a :*: b  -> variant 1 . coarbitrary a . coarbitrary b
         a :-: b  -> variant 2 . coarbitrary a . coarbitrary b
         Negate a -> variant 3 . coarbitrary a
         Con n    -> variant 4 . coarbitrary n
         a :/: b  -> variant 5 . coarbitrary a . coarbitrary b
         Sqrt a   -> variant 6 . coarbitrary a
         Var s    -> variant 7 . coarbitrary s
         Sym f xs -> variant 8 . coarbitrary f . coarbitrary xs
   
arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [liftM (Con . abs) arbitrary, oneof [ return (Var x) | x <- ["x", "y", "z"] ], return pi ]
arbExpr n = oneof [bin (+), bin (*), bin (-), unop negate, bin (/), unop sqrt, arbExpr 0]
 where
   bin  f = liftM2 f rec rec
   unop f = liftM f rec
   rec    = arbExpr (n `div` 2)
       
-----------------------------------------------------------------------
-- Fold

foldExpr (plus, times, minus, neg, con, dv, sq, var, sym) = rec 
 where
   rec expr = 
      case expr of
         a :+: b  -> plus (rec a) (rec b)
         a :*: b  -> times (rec a) (rec b)
         a :-: b  -> minus (rec a) (rec b)
         Negate a -> neg (rec a)
         Con n    -> con n
         a :/: b  -> dv (rec a) (rec b)
         Sqrt a   -> sq (rec a)
         Var v    -> var v
         Sym f xs -> sym f (map rec xs)

exprToNum :: (Monad m, Num a) => Expr -> m a
exprToNum = foldExpr (liftM2 (+), liftM2 (*), liftM2 (-), liftM negate, return . fromInteger, err, err, err, \_ -> err)
 where
   err _ = fail "exprToNum"

exprToFractional :: (Monad m, Fractional a) => Expr -> m a
exprToFractional = foldExpr (liftM2 (+), liftM2 (*), liftM2 (-), liftM negate, return . fromInteger, (/!), err, err, \_ -> err)
 where 
   mx /! my = join (liftM2 safeDivision mx my)
   err _ = fail "exprToFractional"
       
exprToFloating :: (Monad m, Floating a) => (String -> [a] -> m a) -> Expr -> m a
exprToFloating f = foldExpr (liftM2 (+), liftM2 (*), liftM2 (-), liftM negate, return . fromInteger, (/!), liftM sqrt, err, sym)
 where 
   mx /! my = join (liftM2 safeDivision mx my)
   sym s = join . liftM (f s) . sequence 
   err _ = fail "Floating"

safeDivision :: (Monad m, Fractional a) => a -> a -> m a
safeDivision x y = if y==0 then fail "safeDivision" else return (x/y)

-----------------------------------------------------------------------
-- Pretty printer 

instance Show Expr where
   show e = foldExpr (bin "+", bin "*", bin "-", neg, con, bin "/", sq, var, sym) e 0
    where
      con n _     = show n
      var ('_':is) _ = map return ("xyzuvw" ++ ['a'..]) !! read is
      var s _     = s
      neg x b     = parIf (b>0) ("-" ++ x 2)
      sq  x       = sym "sqrt" [x]
      sym s xs b  = parIf (b>1) (unwords (s : map ($ 2) xs))
      bin s x y  b = parIf (b>0) (x 1 ++ s ++ y 1)
      
      parIf b = if b then par else id
      par s   = "(" ++ s ++ ")"
      
      simple (Con n)    = n>=0
      simple (Var _)    = True
      simple (Sym _ xs) = null xs
      simple _          = False
      
instance MetaVar Expr where
   metaVar n = Var ("_" ++ show n)
   isMetaVar (Var ('_':is)) | not (null is) && all isDigit is = Just (read is)
   isMetaVar _ = Nothing
    
instance Constructor Expr where
   constructor = foldExpr (const2 "Plus", const2 "Times", const2 "Minus", const "Neg", const "Con", const2 "Div", const "Sqrt", const "Var", const2 "Sym")
    where const2 = const . const
    
instance ShallowEq Expr where
   shallowEq (Con a)   (Con b)   = a==b
   shallowEq (Var a)   (Var b)   = a==b
   shallowEq (Sym f _) (Sym g _) = f==g
   shallowEq x y = constructor x == constructor y
   
instance UniplateConstr Expr

collectPlus :: Expr -> [Expr]
collectPlus (a :+: b) = collectPlus a ++ collectPlus b
collectPlus e = [e]

collectTimes :: Expr -> [Expr]
collectTimes (a :*: b) = collectTimes a ++ collectTimes b
collectTimes e = [e]

composQ :: Uniplate b => a -> (a -> a -> a) -> (b -> a) -> b -> a
composQ zero combine f = foldr (combine . f) zero . children

size :: Expr -> Int
size e = 1 + composQ 0 (+) size e