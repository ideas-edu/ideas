module Domain.Math.Expr.Data where

import Data.Char  (isDigit, isAlphaNum)
import Data.Ratio
import Test.QuickCheck
import Control.Monad
import Common.Uniplate
import Common.Rewriting hiding (operators)
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols

-----------------------------------------------------------------------
-- Expression data type

data Expr = -- Num 
            Expr :+: Expr 
          | Expr :*: Expr 
          | Expr :-: Expr
          | Negate Expr
          | Nat Integer
            -- Fractional & Floating
          | Expr :/: Expr
          | Sqrt Expr
            -- Symbolic
          | Var String
          | Sym Symbol [Expr]
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
   abs    = unary absSymbol
   signum = unary signumSymbol

instance Fractional Expr where
   (/) = (:/:)
   fromRational r
      | denominator r == 1 = 
           fromIntegral (numerator r)
      | numerator r < 0 =
           Negate (fromIntegral (abs (numerator r)) :/: fromIntegral (denominator r))
      | otherwise = 
           fromIntegral (numerator r) :/: fromIntegral (denominator r)

instance Floating Expr where
   pi      = symbol piSymbol
   sqrt    = Sqrt
   (**)    = binary powerSymbol
   logBase = binary logSymbol
   exp     = unary expSymbol
   log     = unary logSymbol
   sin     = unary sinSymbol
   tan     = unary tanSymbol
   cos     = unary cosSymbol
   asin    = unary asinSymbol
   atan    = unary atanSymbol
   acos    = unary acosSymbol
   sinh    = unary sinhSymbol
   tanh    = unary tanhSymbol
   cosh    = unary coshSymbol
   asinh   = unary asinhSymbol
   atanh   = unary atanhSymbol
   acosh   = unary acoshSymbol 
   
instance Symbolic Expr where
   variable = Var
   
   getVariable (Var s) = return s
   getVariable _       = mzero
   
   function s [a, b] 
      | s == plusSymbol   = a :+: b
      | s == timesSymbol  = a :*: b
      | s == minusSymbol  = a :-: b
      | s == divSymbol    = a :/: b
   function s [a]
      | s == negateSymbol = Negate a
      | s == sqrtSymbol   = Sqrt a
   function s as = 
      Sym s as
   
   getFunction expr =
      case expr of
         a :+: b  -> return (plusSymbol,   [a, b])
         a :*: b  -> return (timesSymbol,  [a, b])
         a :-: b  -> return (minusSymbol,  [a, b])
         Negate a -> return (negateSymbol, [a])
         a :/: b  -> return (divSymbol,    [a, b])
         Sqrt a   -> return (sqrtSymbol,   [a])
         Sym s as -> return (s, as)
         _ -> mzero

-----------------------------------------------------------------------
-- Uniplate instance

instance Uniplate Expr where 
   uniplate expr =
      case getFunction expr of
         Just (s, as) -> (as, \bs -> function s bs)
         _            -> ([], const expr)

-----------------------------------------------------------------------
-- Arbitrary instance

instance Arbitrary Expr where
   arbitrary = natGenerator 
      -- before changing this instance, check that the 
      -- Gaussian elimination exercise still works (with checkExercise)
      {-
      let syms = [plusSymbol, timesSymbol, minusSymbol, negateSymbol, divSymbol]
      in sized (symbolGenerator (const [natGenerator]) syms) -}
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
         Sym f xs -> variant 8 . coarbitrary (show f) . coarbitrary xs
  
symbolGenerator :: (Int -> [Gen Expr]) -> [Symbol] -> Int -> Gen Expr
symbolGenerator extras syms = f 
 where
   f n = oneof $  map (g n) (filter (\s -> n > 0 || arity s == Just 0) syms)
               ++ extras n
   g n s = do
      i  <- case arity s of
               Just i  -> return i
               Nothing -> choose (0, 5)
      as <- replicateM i (f (n `div` i))
      return (function s as)
  
natGenerator :: Gen Expr
natGenerator = liftM (Nat . abs) arbitrary

varGenerator :: [String] -> Gen Expr
varGenerator vars
   | null vars = error "varGenerator: empty list"
   | otherwise = oneof [ return (Var x) | x <- vars ]

-----------------------------------------------------------------------
-- Pretty printer 

instance Show Expr where
   show = showExpr

showExpr :: Expr -> String
showExpr = rec 0 
 where
   rec _ (Nat n) = show n
   rec _ (Var s) 
      | all isAlphaNum s = s
      | otherwise        = "\"" ++ s ++ "\""
   rec i expr = 
      case getFunction expr of
         Just (s, as) -> 
            case (lookup s table, as) of 
               (Just (InfixLeft, n), [x, y]) -> 
                  parIf (i>n) $ concat [rec n x, show s, rec (n+1) y]
               (Just (InfixRight, n), [x, y]) -> 
                  parIf (i>n) $ concat [rec (n+1) x, show s, rec n y]
               (Just (Prefix, n), [x]) -> -- i>=5 prevents "3--5"
                  parIf (i>=n) $ concat [special s, rec (n+1) x]
               _ -> 
                  parIf (not (null as) && i>10000) $ unwords (prefix s : map (rec 10001) as)
         Nothing -> 
            error "showExpr"

   prefix  s = let a = show s in parIf (any (not . isAlphaNum) a) a
   special s = head (extraNames s ++ [symbolName s])

   table  = [ (s, (a, n)) | (n, (a, xs)) <- zip [1..] operators, s <- xs ]

   parIf b = if b then par else id
   par s   = "(" ++ s ++ ")"

instance MetaVar Expr where
   metaVar n = Var ("_" ++ show n)
   isMetaVar (Var ('_':is)) | not (null is) && all isDigit is = Just (read is)
   isMetaVar _ = Nothing

instance ShallowEq Expr where
   shallowEq (Nat a) (Nat b) = a == b
   shallowEq (Var a) (Var b) = a == b
   shallowEq expr1 expr2 =
      case (getFunction expr1, getFunction expr2) of
         (Just (s1, as), Just (s2, bs)) -> 
              s1 == s2 && length as == length bs
         _ -> False 

instance Rewrite Expr

-----------------------------------------------------------------------
-- AC Theory for expression
{-
exprACs :: Operators Expr
exprACs = [plusOperator, timesOperator]

plusOperator, timesOperator :: Operator Expr
plusOperator  = acOperator (+) isPlus
timesOperator = acOperator (*) isTimes

collectPlus, collectTimes :: Expr -> [Expr]
collectPlus  = collectWithOperator plusOperator
collectTimes = collectWithOperator timesOperator

size :: Expr -> Int
size e = 1 + compos 0 (+) size e
-}
collectVars :: Expr -> [String]
collectVars e = [ s | Var s <- universe e ]

hasVars :: Expr -> Bool
hasVars = not . noVars

noVars :: Expr -> Bool
noVars = null . collectVars