-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on UU parsing library)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Parser 
   ( parseMatrix, ppMatrixInContext, ppMatrix, ppMatrixWith, ppRationalMatrix, ppRational
   , parseSystem
   ) where

import UU.Parsing
import UU.Scanner (Pos)
import UU.Parsing.CharParser
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.LinearExpr
import Domain.LinearAlgebra.Equation
import Domain.LinearAlgebra.MatrixRules -- for context
import Common.Context
import Common.Utils
import Common.Exercise
import Data.List
import Data.Char
import GHC.Real

parseSystem :: String -> Either (Doc a) (LinearSystem Rational)
parseSystem input = 
 case runParser (pSystem pRatioParens) input of
    (sys, [])   -> Right sys
    (sys, errs) -> Left (text (show errs)) 
 
pSystem :: Num a => CharParser a -> CharParser (LinearSystem a)
pSystem p = pListSep (pSym '\n') pEquation
 where
   pEquation = (:==:) <$> pTerm <* pToks "==" <*> pTerm
   pTerm  = pChainr ((+) <$ pSym '+') pAtomS
   pAtomS = pSpace *> pAtom <* pSpace
   pAtom  = flip ($) <$> pCon <* pSpace <*> opt ((*) <$ pSym '*' <* pSpace <*> pVar) id <|> pVar
   pVar   = (\x xs -> var (x:xs)) <$> 'a' <..> 'z' <*> pList ('a' <..> 'z' <|> '0' <..> '9')
   pCon   = toLinearExpr <$> p
   pSpace = pList (pAnySym " \t")
 
-----------------------------------------------------------
--- Parser

parseMatrix  :: String -> Either (Doc a) (MatrixInContext Rational)
parseMatrix input =
   case mm of
      Just m | null errs -> Right m
      _                  -> Left (text (show errs))
 where 
   (xs, errs) = runParser (pMatrix pRatioParens) input
   mm = if isRectangular xs then Just (inContext (makeMatrix xs)) else Nothing 

pMatrix :: CharParser a -> CharParser [[a]]
pMatrix p = pListSep (pSym '\n') pRow
 where
   pRow    = pSpaces *> pList1 (p <* pSpaces)
   pSpaces = pList_gr (pAnySym " \t")

pRatioParens :: CharParser Rational
pRatioParens = (opt (negate <$ pSym '-') id <*> pparens pRatio) <|> pRatio

pRatio :: CharParser Rational
pRatio = (\n d -> fromInteger n / fromInteger d) <$> pZ <*> opt (pSym '/' *> pZ1) 1

pZ :: CharParser Integer
pZ = (\f x -> f x) <$> opt (negate <$ pSym '-') id <*> pNat

pZ1 :: CharParser Integer
pZ1 = (\f x -> f x) <$> opt (negate <$ pSym '-') id <*> pNat1

pNat :: CharParser Integer
pNat = read <$> pList1 ('0' <..> '9')

pNat1 :: CharParser Integer
pNat1 = (\x xs -> read (x:xs)) <$> '1' <..> '9' <*> pList ('0' <..> '9')

-- copy/paste, except no white-space is filtered
runParser  :: CharParser a -> String -> (a, [Message Char Pos])
runParser pLogic input = (result, messages)
 where -- quick hack
   steps    = parseString pLogic (safeInit $ unlines $ filter (any (not . isSpace)) $ lines input)
   result   = fstPair (evalSteps steps)
   messages = getMsgs steps
   safeInit xs = if null xs then [] else init xs

fstPair :: Pair a b -> a
fstPair (Pair a b)  =  a
  
pparens :: CharParser a -> CharParser a
pparens = pPacked (pSymLow '(') (pSymLow ')') 
pSymInf a       =  pCostSym   1000 a a
pSymLow a       =  pCostSym      1 a a

-----------------------------------------------------------
--- Pretty-Printer

ppMatrixInContext :: Show a => MatrixInContext a -> String
ppMatrixInContext m = ppStringMatrix (ppFocus m) ++ "\n" ++ ppEnv m

ppEnv :: Show a => MatrixInContext a -> String
ppEnv m = "[" ++ commaList list ++ "]"
 where f s v = s ++ "=" ++ show (get v m)
       list  = [f "covered" covered, f "columnJ" columnJ]

ppFocus :: Show a => MatrixInContext a -> Matrix String
ppFocus c = mapWithPos f (matrix c)
 where f p a
         {-  | focus c==p = "[" ++ show a ++ "]" -}
          | otherwise  = show a
     
ppMatrix :: Show a => Matrix a -> String
ppMatrix = ppMatrixWith show
     
ppMatrixWith :: (a -> String) -> Matrix a -> String
ppMatrixWith f = ppStringMatrix . fmap f 

ppRationalMatrix :: Matrix Rational -> String
ppRationalMatrix = ppMatrixWith ppRational

ppRational :: Rational -> String
ppRational (x :% y)
   | y==1      = show x
   | otherwise = show x ++ "/" ++ show y
        
ppStringMatrix :: Matrix String -> String
ppStringMatrix = format . rows
 where
   format m = let ws = foldr (zipWith max . map length) (repeat 0) m 
                  align i s = take i (s ++ repeat ' ')
              in unlines $ map (concat . intersperse " " . zipWith align ws) m