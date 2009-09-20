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
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.RelationAlgebra.Parser (parseRelAlg, ppRelAlg) where

import Domain.RelationAlgebra.Formula
import Text.Parsing
import Data.Char

myScanner :: Scanner
myScanner = minusAsSpecial $ makeCharsSpecial "~" defaultScanner
   { keywords         = ["V", "E", "I"]
   , keywordOperators = concatMap (map fst . snd) operatorTable
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

parseRelAlg  :: String -> Either SyntaxError (Ranged RelAlg)
parseRelAlg = analyseAndParse pRelAlg . scanWith myScanner

pRelAlg :: Parser Token (Ranged RelAlg)
pRelAlg = pOperators operatorTable pTerm

-- Two postfix operators
pTerm :: Parser Token (Ranged RelAlg)
pTerm = foldl (flip ($)) <$> pAtom <*> pList pUnOp
 where
   pUnOp  =  unaryOp  Inv <$> pKey invSym 
         <|> unaryOp  Not <$> pKey notSym

pAtom :: Parser Token (Ranged RelAlg)
pAtom  =  (\(s, r) -> toRanged (Var s) r) <$> pVarid
      <|> pParens pRelAlg
      <|> toRanged V     <$> pKey "V"
      <|> toRanged empty <$> pKey "E"
      <|> toRanged I     <$> pKey "I"

-----------------------------------------------------------
--- Helper-function for parentheses analyses

analyseAndParse :: Parser Token a -> [Token] -> Either SyntaxError a
analyseAndParse p ts =
   case checkParentheses ts of
      Just err -> Left err
      Nothing  -> case parse p ts of
                     (_, m:_) -> Left (fromMessage m)
                     (a, _)   -> Right a
                                        
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