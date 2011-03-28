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
import Data.Char
import Data.Monoid
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec

type Script = [(Id, String)]
type Annotation = String

-- The parser
parseScript :: FilePath -> IO Script
parseScript file = do
   result <- parseFromFile script file
   case result of
      Left e   -> fail (show e)
      Right xs -> return xs
 `catch` \e -> do print e ; return []

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
   a   <- identifier
   txt <- (singleLineText <|> multiLineText)
   return (a, txt)

singleLineText :: CharParser st String
singleLineText = do 
   lexChar '='
   xs <- manyTill textItem (lexeme (skip newline <|> comment))
   return (concat xs)

multiLineText :: CharParser st String
multiLineText = do 
   lexChar '{'
   xs <- manyTill textItem (lexChar '}')
   return (concat xs)

textItem :: CharParser st String
textItem =  single (noneOf "@#{}")
        <|> try (single escaped)
        <|> annotation
        <|> (comment >> return [])
 where
   single = liftM (\c -> [c])

-- Lexical units
identifier :: CharParser st Id
identifier = lexeme $ do
   xs <- idPart `sepBy1` char '.'
   return (mconcat (map newId xs))
    <?> "identifier"
 where
   idPart   = many1 idLetter
   idLetter = alphaNum <|> oneOf "-_"

annotation :: CharParser st Annotation
annotation = do
   skip (char '@')
   a <- identifier
   return ('@':show a)

escaped :: CharParser st Char
escaped = do
   skip (char '@')
   satisfy (not . isAlphaNum)

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