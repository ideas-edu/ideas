-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on UU parsing library)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Parser 
   ( parseFrac, ppFrac )
   where

import UU.Parsing
import UU.Parsing.CharParser
import UU.Scanner
import Domain.Fraction.Frac
import Data.Char

-----------------------------------------------------------
--- Parser

-- | Parser for logic formulas that respects all associativity and priority laws 
-- | of the constructors
parseFrac  :: String -> (Frac, [Message Char Pos])
parseFrac = runParser pFrac
 where
   pFrac   =  pChainl ((:+:) <$ addSym <|> (:-:) <$ subSym) pFrac'
   pFrac'  =  pChainl ((:*:) <$ mulSym <|> (:/:) <$ divSym) pFrac'' 
   pFrac'' =  Var <$> pVar 
          <|> Con <$> pNat
          <|> toNegOrNot <$> subSym <*> pFrac''
          <|> pparens pFrac

toNegOrNot :: Char -> Frac -> Frac
toNegOrNot _ (Con x) = Con $ negate x
toNegOrNot _ x       = Neg x

pNat :: CharParser Integer
pNat = read <$> pList1 ('0' <..> '9')

--pNeg :: CharParser Frac
--pNeg = (\x -> Con (negate x)) <$> pNat

mulSym = pSym '*'
divSym = pSym '/' 
addSym = pSym '+'
subSym = pSym '-'

fstPair :: Pair a b -> a
fstPair (Pair a _)  =  a

runParser  :: CharParser a -> String -> (a, [Message Char Pos])
runParser pFrac input = (result, messages)
 where
   steps    = parseString pFrac (filter (not . isSpace) input)
   result   = fstPair (evalSteps steps)
   messages = getMsgs steps
   
pparens :: CharParser a -> CharParser a
pparens = pPacked (pSymLow '(') (pSymLow ')')  

pVar :: CharParser String
pVar = pList1 (pAnySymInf ['a'..'z'])    

pAnySymInf xs = foldr1 (<|>) (map pSymInf xs)

pSymInf a       =  pCostSym   1000 a a
pSymLow a       =  pCostSym      1 a a
                                   
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

