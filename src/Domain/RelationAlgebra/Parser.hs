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
module Domain.RelationAlgebra.Parser (parseRelAlg, ppRelAlg) where

import Domain.RelationAlgebra.Formula
import Text.Parsing
import Data.Char

myScanner :: Scanner
myScanner = defaultScanner
   { keywords         = ["V", "E", "I"]
   , keywordOperators = invSym : notSym : concatMap (map fst . snd) operatorTable
   , specialCharacters = "-~" ++ specialCharacters defaultScanner
   }

operatorTable :: OperatorTable RelAlg
operatorTable = 
   [ (RightAssociative, [(orSym, (:||:))])
   , (RightAssociative, [(andSym, (:&&:))])
   , (NoMix,            [(compSym, (:.:)), (addSym, (:+:))])
   ]

andSym  = "/\\"
orSym   = "\\/" 
addSym  = "!"
compSym = ";"
notSym  = "-"
invSym  = "~"
   
-----------------------------------------------------------
--- Parser

parseRelAlg  :: String -> Either SyntaxError RelAlg
parseRelAlg = analyseAndParse pRelAlg . scanWith myScanner

pRelAlg :: Parser Token RelAlg
pRelAlg = pOperators operatorTable pTerm

-- Two postfix operators
pTerm :: Parser Token RelAlg
pTerm = foldl (flip ($)) <$> pAtom <*> pList pUnOp
 where
   pUnOp  =  Inv <$ pKey invSym 
         <|> Not <$ pKey notSym

pAtom :: Parser Token RelAlg
pAtom  =  Var <$> pVarid
      <|> pParens pRelAlg
      <|> const V     <$> pKey "V"
      <|> const empty <$> pKey "E"
      <|> const I     <$> pKey "I"

-----------------------------------------------------------
--- Helper-function for parentheses analyses

analyseAndParse :: Parser Token a -> [Token] -> Either SyntaxError a
analyseAndParse p ts =
   case checkParentheses ts of
      Just err -> Left err
      Nothing  -> either (Left . f) Right (parse p ts)
 where
   f (Just s) = Unexpected s
   f Nothing  = ErrorMessage "Syntax Error"
                                        
-----------------------------------------------------------
--- Pretty-Printer

ppRelAlg :: RelAlg -> String
ppRelAlg = ppRelAlgPrio (0, "")

ppRelAlgPrio :: (Int, String) -> RelAlg -> String 
ppRelAlgPrio n p = foldRelAlg (var, binop 4 ";", binop 4 "!", binop 3 "/\\", binop 2 "\\/", nott, inv, var "V", var "I") p n ""
 where
   binop prio op p q (n, parent) = 
      parIf (n > prio || (prio==4 && n==4 && op/=parent)) (p (prio+1, op) . ((" "++op++" ")++) . q (prio, op))
   var       = const . (++)
   nott p _  = p (6, "") . ("-"++) 
   inv  p _  = p (6, "") . ("~"++)
   parIf b f = if b then ("("++) . f . (")"++) else f