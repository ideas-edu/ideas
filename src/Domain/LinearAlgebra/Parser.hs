-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Parser
   ( parseMatrix, parseVectorSpace, parseSystem
   , ppMatrix, ppMatrixWith
   ) where

import Data.Char
import Data.Either
import Data.List
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.LinearView (isLinear)
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.Vector
import Domain.Math.Data.Relation
import Domain.Math.Expr

parseSystem :: String -> Either String (LinearSystem Expr)
parseSystem input =
   case foreachLine parseEqExpr input of
      Left msg -> Left msg
      Right eqs
         | all f eqs -> Right eqs
         | otherwise -> Left "System is not linear"
        where
          f (a :==: b) = isLinear a && isLinear b

-----------------------------------------------------------
--- Parser

parseMatrix :: String -> Either String (Matrix Expr)
parseMatrix input =
   case foreachLine parseExprTuple input of
      Left msg -> Left msg
      Right xss
         | isRectangular xss -> Right (makeMatrix xss)
         | otherwise         -> Left "Matrix is not rectangular"

parseVectorSpace :: String -> Either String (VectorSpace Expr)
parseVectorSpace input =
   case foreachLine parseExprTuple input of
      Left msg -> Left msg
      Right xss
         | sameDimension vs -> Right (makeVectorSpace vs)
         | otherwise        -> Left "Vectors have different dimensions"
       where
         vs = map fromList xss

nonEmptyLines :: String -> [String]
nonEmptyLines = filter (not . all isSpace) . lines

foreachLine :: (String -> Either String a) -> String -> Either String [a]
foreachLine p input =
   case (partitionEithers . map p . nonEmptyLines) input of
      (msg:_, _) -> Left msg
      ([],   as) -> Right as

-----------------------------------------------------------
--- Pretty-Printer

ppMatrix :: Show a => Matrix a -> String
ppMatrix = ppMatrixWith show

ppMatrixWith :: (a -> String) -> Matrix a -> String
ppMatrixWith f = ppStringMatrix . fmap f

ppStringMatrix :: Matrix String -> String
ppStringMatrix = format . rows
 where
   format m = let ws = foldr (zipWith max . map length) (repeat 0) m
                  align i s = take i (s ++ repeat ' ')
                  par s = "(" ++ s ++ ")"
              in unlines $ map (par . intercalate ", " . zipWith align ws) m