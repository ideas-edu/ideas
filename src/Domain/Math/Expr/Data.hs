{-# OPTIONS -XDeriveDataTypeable #-}
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
module Domain.Math.Expr.Data where

import Data.Char (isAlphaNum)
import Data.Ratio
import Data.Typeable
import Test.QuickCheck
import Control.Monad
import Common.Id
import Common.Uniplate
import Common.Utils (commaList)
import Common.View
import Common.Rewriting
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols
import Text.OpenMath.Symbol

import qualified Common.Rewriting.Term as Term

-----------------------------------------------------------------------
-- Expression data type

data Expr = -- Num 
            Expr :+: Expr 
          | Expr :*: Expr 
          | Expr :-: Expr
          | Negate Expr
          | Nat Integer
            -- Fractional
          | Expr :/: Expr
            -- Floating-point
          | Sqrt Expr
          | Number Double -- positive only
            -- Symbolic
          | Var String
          | Sym Id [Expr]
   deriving (Eq, Ord, Typeable)

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
   pi      = symbol (newId piSymbol)
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

instance WithFunctions Expr where
   function s [a, b] 
      | sameId s plusSymbol   = a :+: b
      | sameId s timesSymbol  = a :*: b
      | sameId s minusSymbol  = a :-: b
      | sameId s divideSymbol = a :/: b
      | isRootSymbol s && b == Nat 2 = Sqrt a
   function s [a]
      | sameId s negateSymbol = Negate a
   function s as = 
      Sym (newId s) as
   
   getFunction expr =
      case expr of
         a :+: b  -> return (newId plusSymbol,   [a, b])
         a :*: b  -> return (newId timesSymbol,  [a, b])
         a :-: b  -> return (newId minusSymbol,  [a, b])
         Negate a -> return (newId negateSymbol, [a])
         a :/: b  -> return (newId divideSymbol, [a, b])
         Sqrt a   -> return (newId rootSymbol,   [a, Nat 2])
         Sym s as -> return (s, as)
         _ -> fail "Expr.getFunction"

instance WithVars Expr where
   variable = Var
   getVariable (Var s) = return s
   getVariable _       = fail "Expr.getVariable"

instance Symbolic Expr

fromDouble :: Double -> Expr
fromDouble d
   | d < 0     = negate (Number (abs d))
   | otherwise = Number d

-----------------------------------------------------------------------
-- Uniplate instance

instance Uniplate Expr where 
   uniplate expr =
      case getFunction expr of
         Just (s, as) -> (as, function s)
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
instance CoArbitrary Expr where      
   coarbitrary expr =
      case expr of 
         a :+: b  -> variant 0 . coarbitrary a . coarbitrary b
         a :*: b  -> variant 1 . coarbitrary a . coarbitrary b
         a :-: b  -> variant 2 . coarbitrary a . coarbitrary b
         Negate a -> variant 3 . coarbitrary a
         Nat n    -> variant 4 . coarbitrary n
         a :/: b  -> variant 5 . coarbitrary a . coarbitrary b
         Number d -> variant 6 . coarbitrary d
         Sqrt a   -> variant 7 . coarbitrary a
         Var s    -> variant 8 . coarbitrary s
         Sym f xs -> variant 9 . coarbitrary (show f) . coarbitrary xs
  
symbolGenerator :: (Int -> [Gen Expr]) -> [(Symbol, Maybe Int)] -> Int -> Gen Expr
symbolGenerator extras syms = f 
 where
   f n = oneof $  map (g n) (filter (\(_, a) -> n > 0 || a == Just 0) syms)
               ++ extras n
   g n (s, arity) = do
      i  <- case arity of
               Just i  -> return i
               Nothing -> choose (0, 5)
      as <- replicateM i (f (n `div` i))
      return (function (newId s) as)
  
natGenerator :: Gen Expr
natGenerator = liftM (Nat . abs) arbitrary

varGenerator :: [String] -> Gen Expr
varGenerator vars
   | null vars = error "varGenerator: empty list"
   | otherwise = oneof [ return (Var x) | x <- vars ]

-----------------------------------------------------------------------
-- Pretty printer 

instance Show Expr where
   show = showExpr operatorTable

showExpr :: OperatorTable -> Expr -> String
showExpr table = rec 0 
 where
   rec _ (Nat n)    = if n>=0 then show n else "(ERROR)" ++ show n
   rec _ (Number d) = if d>=0 then show d else "(ERROR)" ++ show d
   rec _ (Var s) 
      | all isAlphaNum s = s
      | otherwise        = "\"" ++ s ++ "\""
   rec i expr = 
      case getFunction expr of
         Just (s1, [Sym s2 [Var x, a]]) | sameId s1 diffSymbol && sameId s2 lambdaSymbol ->
            parIf (i>10000) $ "D(" ++ x ++ ") " ++ rec 10001 a
         -- To do: remove special case for sqrt
         Just (s, [a, b]) | isRootSymbol s && b == Nat 2 -> 
            parIf (i>10000) $ unwords ["sqrt", rec 10001 a]
         Just (s, xs) | sameId s listSymbol -> 
            "[" ++ commaList (map (rec 0) xs) ++ "]"
         Just (s, as) -> 
            case (lookup s symbolTable, as) of 
               (Just (InfixLeft, n, op), [x, y]) -> 
                  parIf (i>n) $ concat [rec n x, op, rec (n+1) y]
               (Just (InfixRight, n, op), [x, y]) -> 
                  parIf (i>n) $ concat [rec (n+1) x, op, rec n y]
               (Just (InfixNon, n, op), [x, y]) -> 
                  parIf (i>n) $ concat [rec (n+1) x, op, rec (n+1) y]
               (Just (PrefixNon, n, op), [x]) ->
                  parIf (i>=n) $ concat [op, rec (n+1) x]
               _ -> 
                  parIf (not (null as) && i>10000) $ unwords (showSymbol s : map (rec 10001) as)
         Nothing -> 
            error "showExpr"

   showSymbol s
      | isRootSymbol s = "root"
      | otherwise = show s

   symbolTable = [ (newId s, (a, n, op)) | (n, (a, xs)) <- zip [1..] table, (s, op) <- xs ]

   parIf b = if b then par else id
   par s   = "(" ++ s ++ ")"

instance Rewrite Expr

instance Different Expr where
   different = (Nat 0, Nat 1)

instance IsTerm Expr where 
   toTerm (Nat n)    = Term.Num n
   toTerm (Number d) = Term.Float d
   toTerm (Var v)    = Term.Var v
   toTerm expr = 
      case getFunction expr of
         Just (s, xs) -> function s (map toTerm xs)
         Nothing      -> error "IsTerm Expr"

   fromTerm (Term.Num n)   = return (fromInteger n)
   fromTerm (Term.Float d) = return (Number d)
   fromTerm (Term.Var v)   = return (Var v)
   fromTerm t =
      case getFunction t of
         Just (s, xs) -> do
            ys <- mapM fromTerm xs
            return (function s ys)
         _ -> fail "fromTerm"

instance IsTerm a => IsTerm [a] where
   toTerm = function listSymbol . map toTerm
   fromTerm a = do
      xs <- isFunction listSymbol a
      mapM fromTerm xs

toExpr :: IsTerm a => a -> Expr
toExpr a =
   case fromTerm (toTerm a) of
      Just expr -> expr
      Nothing   -> error "Invalid term"

fromExpr :: (MonadPlus m, IsTerm a) => Expr -> m a
fromExpr = fromTerm . toTerm

exprView :: IsTerm a => View Expr a
exprView = makeView fromExpr toExpr

-----------------------------------------------------------------------
-- Utility functions

collectVars :: Expr -> [String]
collectVars e = [ s | Var s <- universe e ]

hasVars :: Expr -> Bool
hasVars = not . noVars

noVars :: Expr -> Bool
noVars = null . collectVars