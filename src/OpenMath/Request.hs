module OpenMath.Request (Request(..), pRequest) where

import OpenMath.StrategyTable
import OpenMath.OMToMatrix
import Domain.LinearAlgebra
import Control.Monad.Error
import Control.Arrow
import Data.List
import Data.Char
import Data.Maybe

------------------------------------------------------------------------
-- Data type for requests

data Request = Request 
   { req_Strategy :: StrategyID 
   , req_Location :: Location
   , req_Term     :: Matrix Rational
   , req_Answer   :: Maybe (Matrix Rational)
   }
 deriving Show
 
----------------------------
-- XML parser for requests

-- ideally, threading of input string should be done in parser monad
type Parser a = String -> Either String (a, String)

pRequest :: Parser Request
pRequest = betweenTags "request" $ \xs -> do
   (sid, xs)    <- betweenTags "strategy" pStrategy xs
   (loc, xs)    <- optional (betweenTags "location" pLocation) xs
   (term, xs)   <- betweenTags "term" pOmObj xs
   (answer, xs) <- optional (betweenTags "answer" pOmObj) xs
   return (Request 
      { req_Strategy = sid 
      , req_Location = fromMaybe [] loc 
      , req_Term     = term 
      , req_Answer   = answer
      }, xs)

-- parse the matrix with integers
pOmObj :: Parser (Matrix Rational)
pOmObj xs = do
   (ys, zs) <- rec xs
   m <- maybe omErr Right (xml2matrix ys)
   return (fmap fromIntegral m, zs)
 where 
   omErr  = Left "OpenMath object"
   rec [] = omErr
   rec xs@(hd:tl) 
      | "</OMOBJ>" `isPrefixOf` xs = 
           return (splitAt 8 xs)
      | otherwise = do
           (a, b) <- rec tl
           return (hd:a, b)

pStrategy :: Parser StrategyID
pStrategy xs
   | null ys   = fail "strategy name"
   | otherwise = return (ys, zs)
 where (ys, zs) = break (\c -> not (isAlphaNum c || c==' ')) xs

pLocation :: Parser Location
pLocation xs =
   case reads xs of
      [(loc, rest)] -> return (loc, rest)
      _             -> fail "location"
   
optional :: Parser a -> Parser (Maybe a)
optional p s = Right $ either (const (Nothing, s)) (first Just) (p s)
      
betweenTags :: String -> Parser a -> Parser a 
betweenTags tag p xs = do
   xs <- spaces xs
   xs <- openTag tag xs
   xs <- spaces xs
   (a, xs) <- p xs
   xs <- spaces xs
   xs <- closeTag tag xs
   xs <- spaces xs
   return (a, xs)

spaces :: Monad m => String -> m String
spaces = return . dropWhile isSpace
   
openTag :: Monad m => String -> String -> m String
openTag tag xs
   | angled tag `isPrefixOf` xs = return $ drop (length tag+2) xs
   | otherwise                  = fail   $ "open tag " ++ show tag

closeTag :: Monad m => String -> String -> m String
closeTag tag xs
   | angled ("/"++tag) `isPrefixOf` xs = return $ drop (length tag+3) xs
   | otherwise                         = fail   $ "close tag " ++ show tag 
   
angled :: String -> String
angled s = "<" ++ s ++ ">"