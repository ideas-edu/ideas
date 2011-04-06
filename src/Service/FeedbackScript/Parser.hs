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
module Service.FeedbackScript.Parser (parseScript, Script) where

import Common.Id
import Control.Monad.Error
import Data.Char
import Data.Monoid
import Service.FeedbackScript.Syntax
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec

-- The parser
parseScript :: FilePath -> IO Script
parseScript file = do
   result <- parseFromFile script file
   case result of
      Left e   -> print e >> return mempty
      Right xs -> return xs

script :: CharParser st Script
script = do
   lexeme (return ())
   xs <- decls
   eof
   return (makeScript xs)

decls :: CharParser st [Decl]
decls = many $ do 
   pos <- getPosition
   guard (sourceColumn pos == 1)
   decl

decl :: CharParser st Decl
decl = do
   dt <- declType
   a  <- identifier
   c  <- optionMaybe (lexChar '|' >> condition)
   t  <- text
   return (Decl dt a c t)
 <|> do
   lexString "namespace"
   liftM NameSpace identifier
 <?> "declaration"

declType :: CharParser st DeclType
declType =  (lexString "text"     >> return RuleText)
        <|> (lexString "string"   >> return StringDecl)
        <|> (lexString "feedback" >> return Feedback)

condition :: CharParser st Condition
condition =
   lexeme (liftM CondRef attribute)
 <|> do
   lexString "recognize"
   lexeme (liftM RecognizedIs identifier)

text :: CharParser st Text
text = do 
   skip (lexChar '=')
   (singleLineText <|> multiLineText)

singleLineText :: CharParser st Text
singleLineText = do 
   manyTill textItem (lexeme (skip newline <|> comment))

multiLineText :: CharParser st Text
multiLineText = do 
   skip (char '{')
   xs <- manyTill (textItem <|> (newline >> return (TextString []))) (lexChar '}')
   return (xs) 

textItem :: CharParser st TextItem
textItem = liftM TextString (many1 (noneOf "@#{}\n" <|> try escaped))
       <|> liftM TextRef attribute
 where
   escaped = skip (char '@') >> satisfy (not . isAlphaNum)

-- Lexical units
identifier :: CharParser st Id
identifier = lexeme (do
   xs <- idPart `sepBy1` char '.'
   return (mconcat (map newId xs))) 
 <?> "identifier"
 where
   idPart   = many1 idLetter
   idLetter = alphaNum <|> oneOf "-_"

attribute :: CharParser st Id
attribute = do
   skip (char '@')
   s <- many1 (alphaNum <|> oneOf "-_") -- identifier?
   return (newId s)
 <?> "attribute"
 
lexChar :: Char -> CharParser s ()
lexChar = skip . lexeme . char

lexString :: String -> CharParser s ()
lexString s = skip (lexeme (try (string s))) <?> "string " ++ show s

comment :: CharParser st ()
comment = skip (char '#' >> manyTill (noneOf "\n") (skip newline <|> eof))

skip :: CharParser st a -> CharParser st ()
skip p = p >> return ()

-- parse white space and comments afterwards   
lexeme :: CharParser s a -> CharParser s a
lexeme p = do 
   a <- p
   skipMany (skip space <|> comment)
   return a