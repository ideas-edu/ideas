-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.RegularExpr.Parser (parseRegExp) where

import Domain.RegularExpr.Expr
import Text.Parsing

letters = ['a' .. 'z'] ++ ['A' .. 'Z']

logicScanner :: Scanner
logicScanner = (specialSymbols ("+*?|" ++ letters) defaultScanner)
   { keywords         = [ [c] | c <- letters ]
   , keywordOperators = ["+", "*", "?", "|"]
   }

parseRegExp :: String -> Either SyntaxError RegExp
parseRegExp input = 
   case parse pRE (scanWith logicScanner input) of
      (a, [])  -> Right a
      (_, m:_) -> Left (fromMessage m)

pRE :: TokenParser RegExp
pRE = pOr 
 where
   pOr   =  pChainl ((:|:) <$ pKey "|") pSeq
   pSeq  =  foldl1 (:*:) <$> pList1 pPost
   pPost =  foldl (flip ($)) <$> pAtom <*> pList pUnop
   pUnop =  Star <$ pKey "*" <|> Plus <$ pKey "+" <|> Option <$ pKey "?"
   pAtom =  pChoice [ const (fromChar c) <$> pKey [c] | c <- letters ]
        <|> pSpec '(' *> pRE <* pSpec ')'
   
   fromChar 'T' = Epsilon
   fromChar 'F' = EmptySet
   fromChar c   = Atom [c]

-- testje = parseRegExp "P+*((QS?)?|R)"