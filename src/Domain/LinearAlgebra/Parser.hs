-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on UU parsing library)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Matrix.Parser (parseMatrix, ppMatrixInContext, ppMatrix) where

import UU.Parsing
import UU.Scanner (Pos)
import Matrix.Domain
import Matrix.Context
import Utils

-----------------------------------------------------------
--- Parser

parseMatrix  :: String -> (Matrix Int, [Message Char Pos])
parseMatrix = undefined

-----------------------------------------------------------
--- Pretty-Printer

ppMatrixInContext :: Show a => MatrixInContext a -> String
ppMatrixInContext m = ppMatrix m ++ "\n" ++ ppEnv m

ppEnv :: Show a => MatrixInContext a -> String
ppEnv m = "[" ++ commaList list ++ "]"
 where f s g = s ++ "=" ++ show (g m)
       list  = [f "covered" covered, f "columnJ" columnJ, f "rowR" rowR, f "value" value]
        
ppMatrix :: Show a => MatrixInContext a -> String
ppMatrix c = format elements
 where
   elements = zipWith (\i -> f (i==curRow c)) [0..] (rows $ matrix c)
   f b = zipWith (\i -> pp (b && i==curColumn c)) [0..]
   pp b a = if b then "[" ++ show a ++ "]" else " " ++ show a ++ " "
   format :: [[String]] -> String
   format m = let ws = foldr (zipWith max . map length) (repeat 0) m 
                  align i s = take i (s ++ repeat ' ')
              in unlines $ map (concat . zipWith align ws) m