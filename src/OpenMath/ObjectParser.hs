-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module OpenMath.ObjectParser 
   ( Expr(..), IsExpr(..), parseExpr, showExpr, xmlToExpr, exprToXML
   ) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Ratio
import OpenMath.Object
import Service.XML

----------------------------------------------------------
-- Open Math data types

-- abstract representation for OM objects
data Expr = Con Integer | Var String 
          | Expr :*: Expr | Expr :+: Expr | Expr :-: Expr | Expr :/: Expr | Negate Expr
          | Matrix [[Expr]] | List [Expr] | Expr :==: Expr | Sqrt Expr | Pi
   deriving Show

----------------------------------------------------------
-- Type class for conversions to and from the Expr datatype

class IsExpr a where
   toExpr   :: a -> Expr
   fromExpr :: Expr -> Maybe a

instance IsExpr Int where
   toExpr = Con . fromIntegral
   fromExpr (Con n) = Just (fromIntegral n)
   fromExpr _       = Nothing

instance IsExpr Integer where
   toExpr = Con
   fromExpr (Con n) = Just n
   fromExpr _       = Nothing 

instance (IsExpr a, IsExpr b) => IsExpr (Either a b) where
   toExpr = either toExpr toExpr
   fromExpr e = 
      let f g = fmap g (fromExpr e)
      in fromMaybe (f Right) (f (Just . Left))

instance IsExpr a => IsExpr [a] where
   toExpr = List . map toExpr
   fromExpr (List xs) = mapM fromExpr xs
   fromExpr _         = Nothing

instance (Integral a, IsExpr a) => IsExpr (Ratio a) where
   toExpr r
      | j == 1    = toExpr i
      | otherwise = toExpr i :/: toExpr j
    where (i, j) = (numerator r, denominator r)
   fromExpr expr = 
      case expr of 
         Negate a -> fmap negate (fromExpr a)
         e1 :/: e2 -> do
            a <- fromExpr e1
            b <- fromExpr e2
            return (a/b)
         _ -> do
            a <- fromExpr expr
            return (toRatio a)

-- local helper function
toRatio :: Integral a => a -> Ratio a
toRatio = fromIntegral

----------------------------------------------------------
-- Parser and pretty printer for OpenMath objects

parseExpr :: String -> Either String Expr
parseExpr input = parseXML input >>= xmlToExpr

showExpr :: Expr -> String
showExpr = showXML . exprToXML

xmlToExpr :: XML -> Either String Expr
xmlToExpr xml = xml2omobj xml >>= omobj2expr

exprToXML :: Expr -> XML
exprToXML = omobj2xml . expr2omobj

----------------------------------------------------------
-- conversion functions: OMOBJ <-> Expr

binaryOps :: [(String, Expr -> Expr -> Expr)]
binaryOps = [ ("times", (:*:)), ("plus" , (:+:)), ("minus", (:-:)), ("divide", (:/:)), ("rational", (:/:)), ("eq", (:==:)) ]

omobj2expr :: OMOBJ -> Either String Expr
omobj2expr omobj =
   case omobj of
      OMI i -> return (Con i)
      OMV v -> return (Var v)
      OMS _ "pi" -> return Pi
      OMA (OMS _ "matrix":rows) -> do
         let f (OMA (OMS _ "matrixrow":elems)) = mapM omobj2expr elems
             f _ = fail "invalid matrix row"
         es <- mapM f rows
         return (Matrix es)
      OMA (OMS _ "list":elements) -> do
         es <- mapM omobj2expr elements
         return (List es)
      OMA [OMS _ "unary_minus", x] ->
         liftM Negate (omobj2expr x)
      OMA [OMS _ "root", x , OMI 2] -> 
         liftM Sqrt (omobj2expr x)
      OMA [OMS _ op, x, y] -> 
         case lookup op binaryOps of
            Just f  -> do
               e1 <- omobj2expr x
               e2 <- omobj2expr y
               return (f e1 e2)
            Nothing -> fail $ "unknown binary operator " ++ show op
      _ -> fail $ "unknown omobj: " ++ show omobj

expr2omobj :: Expr -> OMOBJ
expr2omobj expr =
   case expr of 
      Con i     -> OMI i
      Var v     -> OMV v
      x :*: y   -> binop "arith1" "times"  x y
      x :+: y   -> binop "arith1" "plus"   x y
      x :-: y   -> binop "arith1" "minus"  x y
      x :/: y   -> binop "arith1" "divide" x y
      Negate x  -> OMA [OMS "arith1" "unary_minus", expr2omobj x]
      Sqrt x    -> binop "arith1" "root" x (Con 2)
      Pi        -> OMS "nums1" "pi"
      x :==: y  -> binop "relation1" "eq"  x y
      List xs   -> OMA (OMS "list1" "list" : map expr2omobj xs)
      Matrix xs ->
         let f ys = OMA (OMS "linalg2" "matrixrow" : map expr2omobj ys)
         in OMA (OMS "linalg2" "matrix" : map f xs)
      
binop :: String -> String -> Expr -> Expr -> OMOBJ
binop cd name x y = OMA [OMS cd name, expr2omobj x, expr2omobj y]

----------------------------------------------------------
-- Test function
{-
check :: IO ()
check = do
   input <- readFile "src/OpenMath/omobj3" 
   let Right expr = parseExpr input 
   print expr
   let new = showXML $ omobj2xml $ expr2omobj expr
   putStrLn new
   print $ parseExpr new 
-}