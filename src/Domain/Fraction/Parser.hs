-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Parser 
   ( parseFrac, ppFrac )
   where

import Common.Parsing hiding (pParens)
import Domain.Fraction.Frac
import Data.Char

scannerExpr :: Scanner
scannerExpr = defaultScanner {specialCharacters  = "+-*/()[]{},"}

operatorTable :: OperatorTable Frac
operatorTable = 
   [ (LeftAssociative, [("+", (:+:)), ("-", (:-:))])  -- infixl 6
   , (LeftAssociative, [("*", (:*:)), ("/", (:/:))])  -- infixl 7
   ] 
   
-----------------------------------------------------------
--- Parser

-- | Parser for logic formulas that respects all associativity and priority laws 
-- | of the constructors
parseFrac :: String -> Either SyntaxError Frac
parseFrac = f . parse pExpr . scanWith scannerExpr
 where 
   f (e, []) = Right e
   f (_, xs) = Left $ ErrorMessage $ unlines $ map show xs
   
pExpr :: TokenParser Frac
pExpr = fromRanged <$> pOperators operatorTable (flip toRanged nul <$> pTerm)

pTerm :: TokenParser Frac
pTerm = optional (Neg <$ pKey "-") id <*> pAtom

pAtom :: TokenParser Frac
pAtom  =  Con <$> pInteger
      <|> (Var . fst) <$> (pVarid <|> pConid)
      <|> pParens pExpr
      
pParens :: TokenParser a -> TokenParser a
pParens p = pKey "(" *> p <* pKey ")"

nul :: Range
nul = Range (Pos 0 0) (Pos 0 0)

-----------------------------------------------------------
--- Pretty-Printer

ppFrac :: Frac -> String
ppFrac =  ppFracParsCode 0
        
-- | Implementation uses the well-known trick for fast string concatenation
ppFracParsCode :: Int -> Frac -> String
ppFracParsCode n p = foldFrac (var, lit, binop 2 "*", binop 2 "/", binop 3 "+", binop 3 "-", neg) p n ""
 where
   binop prio op p q n = parIf True (p prio . ((" "++op++" ")++) . q prio)
   var       = const . (++)
   lit       = const . (++) . show
   neg  p n = ("-"++) .(parIf True (p 3))
   parIf b f = if b then ("("++) . f . (")"++) else f

