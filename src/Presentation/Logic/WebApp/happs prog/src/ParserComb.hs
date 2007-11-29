module ParserComb (Parser,
                  (<|>),(<*>),(<$>),
                  many,many1,option, succeed,
                  chainr, chainl,
                  first, greedy, greedy1,
                  dropspaces, symbol, satisfy,
                  identifier, digit, spaces,
                  natural, newdigit, integer, fixed,
                  list, listOf, token, parenthesised) where

-- Author  : Harrie Passier
-- Date    : March 208,2005
-- Version : 1

import List
import Char
import Int

infixr 4 <|>
infixl 6 <*>
infixr 7 <$>


-- Parser combinators

type Parser b a = [b] -> [(a,[b])]

(<|>)        :: Parser s a -> Parser s a -> Parser s a
(p <|> q) xs =  p xs ++ q xs


(<*>)        :: Parser s (b -> a) -> Parser s b -> Parser s a
(p <*> q) xs =  [(f x,zs)
                | (f ,ys) <- p xs
                , ( x,zs) <- q ys
                ]

(<$>)        :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) xs =  [(f y,ys)
                |(  y,ys) <- p xs
                ]

-- General parsers

many   :: Parser s a -> Parser s [a]
many p =  list <$> p <*> many p <|> succeed []

many1   :: Parser s a -> Parser s [a]
many1 p =  list <$> p <*> many p

option     :: Parser s a -> a -> Parser s a
option p d =  p <|> succeed d

succeed      :: a -> Parser s a
succeed r xs = [(r,xs)]

chainr :: Parser s a -> Parser s (a->a->a) -> Parser s a
chainr pe po = h <$> many (j <$> pe <*> po) <*> pe
   where j x op = ( x `op`)
         h fs x = foldr ($) x fs 

chainl :: Parser s a -> Parser s (a->a->a) -> Parser s a
chainl pe po = h <$> pe <*> many (j <$> po <*> pe)
  where j op x = (`op` x)
        h x fs = foldl (flip ($)) x fs

symbol  :: Eq s => s -> Parser s s
symbol a []                 = []
symbol a (x:xs) | x == a    = [(x,xs)]
                | otherwise = []

satisfy :: (s -> Bool) -> Parser s s
satisfy p []                = []
satisfy p (x:xs) | p x       = [(x,xs)]
                 | otherwise = []

first :: Parser s b -> Parser s b
first p xs | null r  = []
           | otherwise = [head r]
  where r = p xs

greedy, greedy1 :: Parser s b -> Parser s [b]
greedy  = first . many
greedy1 = first . many1

identifier :: Parser Char String 
identifier =  list <$> satisfy isAlpha <*> greedy (satisfy isAlphaNum)

digit :: Parser Char Char
digit = satisfy isDigit

spaces :: Parser Char String
spaces = many (symbol ' ')

dropspaces :: Parser Char a -> Parser Char a 
dropspaces p = p . dropWhile (==' ')  

natural :: Parser Char Int
natural = foldl f 0 <$> many1 newdigit
  where f a b = a*10 + b

newdigit :: Parser Char Int
newdigit = f <$> digit
   where f c = ord c - ord '0'

integer :: Parser Char Int
integer =  (const negate <$> (symbol '-')) `option` id <*> natural  

 
fixed :: Parser Char Float
fixed = (+) <$> ((fromInteger . toInteger . head) <$> greedy1 integer)
                <*> (((\x y -> y) <$> symbol '.' <*> fractpart ) `option` 0.0)

fractpart :: Parser Char Float
fractpart =  (foldr f 0.0) <$> greedy newdigit
  where f d n = (n + (fromInteger . toInteger) d)/10.0  


listOf     :: Parser s a -> Parser s b -> Parser s [a]
listOf p s =  list <$> p <*> many ((\x y -> y) <$> s <*> p)

token :: Eq s => [s] -> Parser s [s]
token k xs | k == take n xs = [(k, drop n xs)]
           | otherwise      = []
    where n = length k 

pack       :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack p r q =  (\x y z -> y) <$> p <*> r <*> q


parenthesised p = pack (symbol '(') p (symbol ')') 



-- Overige functies

list x xs = x:xs




