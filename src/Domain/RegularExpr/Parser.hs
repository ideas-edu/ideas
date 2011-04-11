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
module Domain.RegularExpr.Parser (parseRegExp) where

import Domain.RegularExpr.Expr
import Text.Parsing
import qualified Text.ParserCombinators.Parsec.Token as P

parseRegExp :: String -> Either String RegExp
parseRegExp = parseSimple pRE 
 where
   pRE  = chainl1 pSeq ((:|:) <$ reservedOp "|")
   pSeq = foldl1 (:*:) <$> many1 post
   post = foldl (flip ($)) <$> atom <*> many pUn
   pUn  = choice 
      [ Star   <$ reservedOp "*" 
      , Plus   <$ reservedOp "+" 
      , Option <$ reservedOp "?" 
      ]
   atom = choice
      [ Epsilon  <$ P.reserved lexer "T"
      , EmptySet <$ P.reserved lexer "F"
      , Atom <$> P.identifier lexer
      , P.parens lexer pRE
      ]

lexer :: P.TokenParser a
lexer = P.makeTokenParser $ emptyDef 
   { reservedNames   = ["T", "F"]
   , reservedOpNames = ["+", "*", "?", "|"]
   , identStart      = letter
   , identLetter     = fail "" 
   , opStart         = fail "" 
   , opLetter        = fail ""
   }

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

--  testje = parseRegExp "P+*((QS?)?|R)"