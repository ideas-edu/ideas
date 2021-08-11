-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Simple parser for feedback scripts
--
-----------------------------------------------------------------------------

module Ideas.Service.FeedbackScript.Parser
   ( parseScript, parseScriptSafe, Script
   ) where

import Control.Exception hiding (try)
import Control.Monad
import Data.Char
import Data.List
import Ideas.Common.Id
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Utils.Parsing
import System.Directory
import System.FilePath

-- returns the empty script if something goes wrong
parseScriptSafe :: FilePath -> IO Script
parseScriptSafe file = parseScript file `catch` handler
 where
   handler :: SomeException -> IO Script
   handler _ = return mempty

-- chases all included script files
parseScript :: FilePath -> IO Script
parseScript file = rec [] [file]
 where
   rec _ [] = return mempty
   rec hist (a:as)
      | a `elem` hist = rec hist as
      | otherwise = do
           s1 <- parseOneScriptFile a
           let new = map (replaceFileName file) (includes s1)
           s2 <- rec (a:hist) (new++as) -- depth-first
           return (s1 <> s2) -- included parts are inserted at the end

parseOneScriptFile :: FilePath -> IO Script
parseOneScriptFile file = do
   result <- parseFromFile script file `catch` handler
   case result of
      Left e   -> print e >> return mempty
      Right xs -> return xs
 where
   -- on failure, visit scripts directory (if this directory exists)
   handler :: IOException -> IO (Either ParseError Script)
   handler io = do
      b <- doesDirectoryExist "scripts"
      if b && not ("scripts" `isPrefixOf` file)
         then parseFromFile script ("scripts/" ++ file)
         else throw io

script :: Parser Script
script = makeScript <$> complete decls

decls :: Parser [Decl]
decls = many $ do
   pos <- getPosition
   guard (sourceColumn pos == 1)
   decl

decl :: Parser Decl
decl = do
   dt <- declType
   a  <- identifiers
   f  <- simpleDecl <|> guardedDecl
   return (f dt a)
 <|>
   NameSpace <$ lexString "namespace" <*>  identifiers
 <|>
   Supports <$ lexString "supports" <*> identifiers
 <|>
   Include <$ lexString "include" <*> filenames
 <?> "declaration"

simpleDecl, guardedDecl :: Parser (DeclType -> [Id] -> Decl)
simpleDecl  =  (\t dt a -> Simple dt a t)
           <$> text
guardedDecl =  (\xs dt a -> Guarded dt a xs)
           <$> many1 ((,) <$> (lexChar '|' *> condition) <*> text)

declType :: Parser DeclType
declType =  (TextForId  <$ lexString "text")
        <|> (StringDecl <$ lexString "string")
        <|> (Feedback   <$ lexString "feedback")

condition :: Parser Condition
condition = choice
   [ CondRef         <$> lexeme attribute
   , RecognizedIs    <$  lexString "recognize" <*> identifier
   , MotivationIs    <$  lexString "motivation" <*> identifier
   , CondConst True  <$  lexString "true"
   , CondConst False <$  lexString "false"
   , CondNot         <$  lexString "not" <*> condition
   ]

text :: Parser Text
text = lexChar '=' *> (singleLineText <|> multiLineText)

singleLineText :: Parser Text
singleLineText =
   mconcat <$> manyTill textItem (lexeme (skip newline <|> comment))

multiLineText :: Parser Text
multiLineText =
   mconcat <$  char '{'
           <*> manyTill (textItem <|> (mempty <$ newline)) (lexChar '}')

textItem :: Parser Text
textItem = makeText <$> many1 (noneOf "@#{}\n" <|> try escaped)
       <|> TextRef  <$> attribute
 where
   escaped = char '@' *> satisfy (not . isAlphaNum)

identifiers :: Parser [Id]
identifiers = sepBy1 identifier (lexChar ',')

-- Lexical units
identifier :: Parser Id
identifier = lexeme (mconcat . map newId <$> idPart `sepBy1` char '.')
 <?> "identifier"
 where
   idPart   = many1 idLetter
   idLetter = alphaNum <|> oneOf "-_"

attribute :: Parser Id
attribute = newId <$ skip (char '@') <*>  many1 (alphaNum <|> oneOf "-_")
   <?> "attribute"

filenames :: Parser [FilePath]
filenames = sepBy1 filename (lexChar ',')

filename :: Parser FilePath
filename = lexeme $ many1 (alphaNum <|> oneOf "+-_./\\:;|")

lexChar :: Char -> Parser ()
lexChar = skip . lexeme . char

lexString :: String -> Parser ()
lexString s = skip (lexeme (try (string s))) <?> "string " ++ show s

comment :: Parser ()
comment = char '#' *> many (satisfy (/= '\n')) *> (skip newline <|> eof)

-- parse white space and comments afterwards
lexeme :: Parser a -> Parser a
lexeme p = p <* skipMany (skip space <|> comment)