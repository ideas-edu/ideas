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
-- Simple parser for feedback scripts
--
-----------------------------------------------------------------------------
module Service.ScriptParser (Script, parseScript) where

import Common.Id
import Control.Monad.Error
import Data.Monoid
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec

type Script = [(Id, String)]

-- The parser
parseScript :: FilePath -> IO Script
parseScript file = do
   result <- parseFromFile script file
   case result of
      Left e   -> fail (show e)
      Right xs -> return xs

script :: CharParser st Script
script = do
   lexeme (return ())
   xs <- decls
   eof
   return xs

decls :: CharParser st Script
decls = many decl

decl :: CharParser st (Id, String)
decl = do
   beginDecl
   a <- identifier
   lexChar ':'
   txt <- freeText
   return (a, txt)

freeText :: CharParser st String
freeText = manyTill (liftM (const ' ') comment <|> anyChar) (beginDecl <|> eof)

-- Lexical units
beginDecl :: CharParser st ()
beginDecl = do
   pos <- getPosition
   guard (sourceColumn pos == 1)
   notFollowedBy space

identifier :: CharParser st Id
identifier = lexeme $ do
   xs <- idPart `sepBy1` char '.'
   return (mconcat (map newId xs))
    <?> "identifier"
 where
   idPart   = many1 idLetter
   idLetter = alphaNum <|> oneOf "-_"

lexChar :: Char -> CharParser s ()
lexChar c = skip (lexeme (char c))

comment :: CharParser st ()
comment = skip (char '#' >> manyTill (noneOf "\n") (skip newline <|> eof))

skip :: CharParser st a -> CharParser st ()
skip p = p >> return ()

-- parser white space and comments afterwards   
lexeme :: CharParser s a -> CharParser s a
lexeme p = do 
   a <- p
   skipMany (skip space <|> comment)
   return a