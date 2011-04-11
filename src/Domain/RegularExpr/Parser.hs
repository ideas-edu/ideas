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

import Control.Arrow
import Control.Monad
import Domain.RegularExpr.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

parseRegExp :: String -> Either String RegExp
parseRegExp = parseWith pRE 
 where
   pRE  = chainl1 pSeq (reservedOp "|" >> return (:|:))
   pSeq = liftM (foldl1 (:*:)) (many1 post)
   post = liftM2 (foldl (flip ($))) atom (many pUn)
   pUn  = choice 
      [ reservedOp "*" >> return Star
      , reservedOp "+" >> return Plus
      , reservedOp "?" >> return Option
      ]
   atom = choice
      [ P.reserved lexer "T" >> return Epsilon
      , P.reserved lexer "F" >> return EmptySet
      , liftM Atom (P.identifier lexer)
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

parseWith :: Parser a -> String -> Either String a
parseWith p = left show . runParser start () ""
 where
   start = (P.whiteSpace lexer) >> p >>= \a -> eof >> return a
   
--  testje = parseRegExp "P+*((QS?)?|R)"