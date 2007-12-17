module OpenMath.LAServer where

import Domain.LinearAlgebra
import OpenMath.OMToMatrix
import Common.Transformation
import Data.Char
import Data.List

data Request a = Request (Matrix a) (Matrix a) StrategyID Location
   deriving Show

data Reply a = Ok 
             | ParseError
             | Incorrect (Matrix a) MDQuestion StrategyID Location
   deriving Show

type StrategyID = String
type Location   = [Int]
type MDQuestion = String

q = do 
   input <- readFile "request"
   putStrLn input
   let Just req = toRequest input
   putStrLn (show req)
   let repl = fromReply $ laServer req
   putStrLn repl
   writeFile "reply" repl 
  
----------------------------

respond :: String -> String
respond = fromReply . maybe ParseError laServer . toRequest

toRequest :: String -> Maybe (Request Int)
toRequest = fmap fst . pRequest
 
fromReply :: Reply Int -> String
fromReply reply =
   case reply of
      Ok -> "<reply result=\"ok\"/>"
      ParseError -> "<reply result=\"parse error\"/>"
      Incorrect m question sid loc -> unlines
         [ "<reply><term>"
         , matrix2xml m
         , "</term><question>"
         , question
         , "</question><strategy>"
         , sid
         , "</strategy><location>"
         , show loc
         , "</location></reply>"
         ]
      
----------------------------
-- Top-level XML Parser

type Parser a = String -> Maybe (a, String)

pRequest :: Parser (Request Int)
pRequest = betweenTags "request" $ \xs -> do
   (a, xs) <- betweenTags "term" pOmObj xs
   (b, xs) <- betweenTags "answer" pOmObj xs
   (c, xs) <- betweenTags "strategy" pStrategy xs
   (d, xs) <- betweenTags "location" pLocation xs
   let m = makeMatrix [[0,1],[1,0]]
   return (Request a b c d, xs)

pOmObj :: Parser (Matrix Int)
pOmObj xs = do 
   m <- xml2matrix ys
   return (m, zs)
 where (ys, zs) = rec xs 
       rec xs@(hd:tl) 
          | "</OMOBJ>" `isPrefixOf` xs = splitAt 8 xs 
          | otherwise                  = let (a, b) = rec tl
                                         in (hd:a, b)

pStrategy :: Parser StrategyID
pStrategy xs
   | null ys   = Nothing
   | otherwise = Just (ys, zs)
 where (ys, zs) = break (not . isAlphaNum) xs

pLocation :: Parser Location
pLocation xs =
   case reads xs of
      [(loc, rest)] -> return (loc, rest)
      _             -> Nothing

betweenTags :: String -> Parser a -> Parser a 
betweenTags tag p xs = do
   xs <- spaces xs
   (_, xs) <- openTag tag xs
   xs <- spaces xs
   (a, xs) <- p xs
   xs <- spaces xs
   (_, xs) <- closeTag tag xs
   xs <- spaces xs
   return (a, xs)
 where 
   spaces = return . dropWhile isSpace
   
openTag :: String -> Parser ()
openTag tag xs
   | angled tag `isPrefixOf` xs = Just ((), drop (length tag+2) xs)
   | otherwise                  = Nothing

closeTag :: String -> Parser ()
closeTag tag xs
   | angled ("/"++tag) `isPrefixOf` xs = Just ((), drop (length tag+3) xs)
   | otherwise                         = Nothing
   
angled :: String -> String
angled s = "<" ++ s ++ ">"

----------------------------

laServer :: Request Int -> Reply Int
laServer (Request term answer strategy location)
   | fmap toRational answer == correct = Ok
   | otherwise = Incorrect term "Incorrect: try again." strategy location
 where
   correct = matrix $ applyD toReducedEchelon $ inContext $ fmap toRational term