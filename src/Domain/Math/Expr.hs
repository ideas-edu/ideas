module Domain.Math.Expr where

import Data.Char  (isAlpha, isDigit)
import Data.Ratio
import Test.QuickCheck
import Control.Monad
import Common.Uniplate
import Common.Rewriting
import Domain.Math.Symbolic

-----------------------------------------------------------------------
-- Expression data type

data Expr = -- Num 
            Expr :+: Expr 
          | Expr :*: Expr 
          | Expr :-: Expr
          | Negate Expr
          | Nat Integer
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
   fromInteger n 
      | n < 0     = negate $ Nat $ abs n
      | otherwise = Nat n
   negate = Negate 
   abs    = unaryFunction "abs"
   signum = unaryFunction "signum"

instance Fractional Expr where
   (/) = (:/:)
   fromRational r
      | denominator r == 1 = fromIntegral (numerator r)
      | otherwise = fromIntegral (numerator r) :/: fromIntegral (denominator r)

instance Floating Expr where
   pi      = symbol "pi"
   sqrt    = Sqrt
   (**)    = binaryFunction "**"
   logBase = binaryFunction "logBase"
   exp     = unaryFunction "exp"
   log     = unaryFunction "log"
   sin     = unaryFunction "sin"
   tan     = unaryFunction "tan"
   cos     = unaryFunction "cos"
   asin    = unaryFunction "asin"
   atan    = unaryFunction "atan"
   acos    = unaryFunction "acos"
   sinh    = unaryFunction "sinh"
   tanh    = unaryFunction "tanh"
   cosh    = unaryFunction "cosh"
   asinh   = unaryFunction "asinh"
   atanh   = unaryFunction "atanh"
   acosh   = unaryFunction "acosh" 
   
instance Symbolic Expr where
   variable = Var
   function = Sym

infixr 8 ^

(^) :: Symbolic a => a -> a -> a
(^) = binaryFunction "^" 

bottom :: Expr
bottom = symbol "_|_"

-----------------------------------------------------------------------
-- Uniplate instance

instance Uniplate Expr where 
   uniplate expr =
      case expr of
         a :+: b  -> ([a,b], \[x,y] -> x :+: y)
         a :*: b  -> ([a,b], \[x,y] -> x :*: y)
         a :-: b  -> ([a,b], \[x,y] -> x :-: y)
         Negate a -> ([a]  , \[x]   -> Negate x)
         Nat _    -> ([]   , \[]    -> expr)
         a :/: b  -> ([a,b], \[x,y] -> x :/: y)
         Sqrt a   -> ([a]  , \[x]   -> Sqrt x)
         Var _    -> ([]   , \[]    -> expr)
         Sym s as -> (as   , \xs    -> Sym s xs)

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
         Nat n    -> variant 4 . coarbitrary n
         a :/: b  -> variant 5 . coarbitrary a . coarbitrary b
         Sqrt a   -> variant 6 . coarbitrary a
         Var s    -> variant 7 . coarbitrary s
         Sym f xs -> variant 8 . coarbitrary f . coarbitrary xs
   
arbExpr :: Int -> Gen Expr
arbExpr _ = liftM (Nat . abs) arbitrary
{-
arbExpr 0 = oneof [liftM (Nat . abs) arbitrary, oneof [ return (Var x) | x <- ["x", "y", "z"] ] {- , return pi -} ]
arbExpr n = oneof [bin (+), bin (*), bin (-), unop negate, bin (/), unop sqrt, arbExpr 0]
 where
   bin  f = liftM2 f rec rec
   unop f = liftM f rec
   rec    = arbExpr (n `div` 2) -}
       
-----------------------------------------------------------------------
-- Fold

foldExpr (plus, times, minus, neg, nat, dv, sq, var, sym) = rec 
 where
   rec expr = 
      case expr of
         a :+: b  -> plus (rec a) (rec b)
         a :*: b  -> times (rec a) (rec b)
         a :-: b  -> minus (rec a) (rec b)
         Negate a -> neg (rec a)
         Nat n    -> nat n
         a :/: b  -> dv (rec a) (rec b)
         Sqrt a   -> sq (rec a)
         Var v    -> var v
         Sym f xs -> sym f (map rec xs)

exprToNum :: (Monad m, Num a) => Expr -> m a
exprToNum = foldExpr (liftM2 (+), liftM2 (*), liftM2 (-), liftM negate, return . fromInteger, \_ -> err, err, err, \_ -> err)
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
   show = ppExprPrio 0

ppExprPrio :: Int -> Expr -> String
ppExprPrio = flip $ foldExpr (bin "+" 1, bin "*" 2, bin "-" 1, neg, nat, bin "/" 2, sq, var, sym)
 where
   nat n b        = if n>=0 then show n else neg (nat (abs n)) b
   var s _        = s
   neg x b        = parIf (b>0) ("-" ++ x 10)
   sq  x          = sym "sqrt" [x]
   sym s xs b
      | null xs   = s
      | length xs==2 && not (isAlpha (head s)) 
                  = bin s 0 (xs!!0) (xs!!1) 1
      | otherwise = parIf (b>3) (unwords (s : map ($ 4) xs))
   bin s i x y b  = parIf (b>i) (x i ++ s ++ y (i+1))
      
   parIf b = if b then par else id
   par s   = "(" ++ s ++ ")"
    
instance MetaVar Expr where
   metaVar n = Var ("_" ++ show n)
   isMetaVar (Var ('_':is)) | not (null is) && all isDigit is = Just (read is)
   isMetaVar _ = Nothing

instance ShallowEq Expr where
   shallowEq expr1 expr2 =
      case (expr1, expr2) of
         (_ :+: _ , _ :+: _ ) -> True
         (_ :*: _ , _ :*: _ ) -> True
         (_ :-: _ , _ :-: _ ) -> True
         (Negate _, Negate _) -> True
         (Nat a   , Nat b   ) -> a==b
         (_ :/: _ , _ :/: _ ) -> True
         (Sqrt _  , Sqrt _  ) -> True
         (Var a   , Var b   ) -> a==b
         (Sym f _ , Sym g _ ) -> f==g
         _                    -> False
   
instance Rewrite Expr

-----------------------------------------------------------------------
-- AC Theory for expression

exprACs :: Operators Expr
exprACs = [plusOperator, timesOperator]

plusOperator :: Operator Expr
plusOperator = acOperator (+) isPlus
 where
   isPlus (a :+: b) = Just (a, b)
   isPlus _         = Nothing

timesOperator :: Operator Expr
timesOperator = acOperator (*) isTimes
 where
   isTimes (a :*: b) = Just (a, b)
   isTimes _         = Nothing

collectPlus, collectTimes :: Expr -> [Expr]
collectPlus  = collectWithOperator plusOperator
collectTimes = collectWithOperator timesOperator

size :: Expr -> Int
size e = 1 + compos 0 (+) size e

collectVars :: Expr -> [String]
collectVars e = [ s | Var s <- universe e ]

hasVars :: Expr -> Bool
hasVars = not . noVars

noVars :: Expr -> Bool
noVars = null . collectVars

substituteVars :: (String -> Expr) -> Expr -> Expr
substituteVars sub = rec 
 where
   rec e@(Var s) = sub s
   rec e = f (map rec cs)
    where (cs, f) = uniplate e