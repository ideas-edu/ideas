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
module Service.ScriptParser (parseScript) where

import Common.Id
import Control.Monad.Error
import Data.Char
import Data.Monoid
import Service.FeedbackScript
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec

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

decl :: CharParser st Decl
decl = do
   lexString "text"
   a <- identifier
   t <- (singleLineText <|> multiLineText)
   return (RuleText a t)
 <|> do
   lexString "string"
   a <- identifier
   t <- (singleLineText <|> multiLineText)
   return (StringDecl a t)
 <|> do
   lexString "feedback"
   a <- identifier
   t <- (singleLineText <|> multiLineText)
   return (Feedback a t)

singleLineText :: CharParser st Text
singleLineText = do 
   lexChar '='
   xs <- manyTill textItem (lexeme (skip newline <|> comment))
   return (mconcat xs)

multiLineText :: CharParser st Text
multiLineText = do 
   lexChar '{'
   xs <- manyTill textItem (lexChar '}')
   return (mconcat xs)

textItem :: CharParser st Text
textItem = single (noneOf "@#{}")
       <|> try (single escaped)
       <|> attribute
       <|> (comment >> return mempty)
 where
   single = liftM (\c -> Text [c])

-- Lexical units
identifier :: CharParser st Id
identifier = lexeme $ do
   xs <- idPart `sepBy1` char '.'
   return (mconcat (map newId xs))
    <?> "identifier"
 where
   idPart   = many1 idLetter
   idLetter = alphaNum <|> oneOf "-_"

attribute :: CharParser st Text
attribute = do
   skip (char '@')
   s <- many1 (alphaNum <|> oneOf "-_") -- identifier?
   return (AttrRef (newId s))

escaped :: CharParser st Char
escaped = do
   skip (char '@')
   satisfy (not . isAlphaNum)

lexChar :: Char -> CharParser s ()
lexChar c = skip (lexeme (char c))

lexString :: String -> CharParser s ()
lexString s = skip (lexeme (string s))

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