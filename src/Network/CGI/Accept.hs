module Network.CGI.Accept (
  -- * Accept-X headers
    Acceptable
  , Accept
  , Charset(..), ContentEncoding(..), Language(..)
  -- * Content negotiation
  , negotiate
                          ) where

import Data.Function
import Data.List
import Data.Maybe
import Numeric

import Text.ParserCombinators.Parsec

import Network.Multipart
import Network.Multipart.Header

--
-- * Accept-X headers
--

newtype Accept a = Accept [(a, Quality)]
    deriving (Show)

type Quality = Double

-- A bounded join-semilattice
class Eq a => Acceptable a where
    includes :: a -> a -> Bool
    top :: a

instance HeaderValue a => HeaderValue (Accept a) where
    parseHeaderValue = fmap Accept $ sepBy p (lexeme (char ','))
        where p = do a <- parseHeaderValue
                     q <- option 1 $ do _ <- lexeme $ char ';'
                                        _ <- lexeme $ char 'q'
                                        _ <- lexeme $ char '='
                                        lexeme pQuality
                     return (a,q)
              pQuality = (char '0' >> option "0" (char '.' >> many digit) >>= \ds -> return (read ("0." ++ ds ++ "0")))
                         <|> (char '1' >> optional (char '.' >> many (char '0')) >> return 1)
    prettyHeaderValue (Accept xs) = concat $ intersperse ", " [prettyHeaderValue a ++ "; q=" ++ showQuality q | (a,q) <- xs]
        where showQuality q = showFFloat (Just 3) q ""

starOrEqualTo :: String -> String -> Bool
starOrEqualTo x y = x == "*" || x == y

negotiate :: Acceptable a => [a] -> Maybe (Accept a) -> [a]
negotiate ys Nothing = ys
negotiate ys (Just xs) = reverse [ z | (q,z) <- sortBy (compare `on` fst) [ (quality xs y,y) | y <- ys], q > 0]

--testNegotiate :: (HeaderValue a, Acceptable a) => [String] -> String -> [a]
--testNegotiate ts a = negotiate [t | Just t <- map (parseM parseHeaderValue "<source>") ts] (parseM parseHeaderValue "<source>" a)

quality :: Acceptable a => Accept a -> a -> Quality
quality (Accept xs) y = fromMaybe 0 $ listToMaybe $ sort $ map snd $ sortBy (compareSpecificity `on` fst) $ filter ((`includes` y) . fst) xs

compareSpecificity :: Acceptable a => a -> a -> Ordering
compareSpecificity x y
    | x `includes` y && y `includes` x = EQ
    | x `includes` y = GT
    | y `includes` x = LT
    | otherwise = error "Non-comparable Acceptables"

--
-- ** Accept
--

instance Acceptable ContentType where
    includes x y = ctType x `starOrEqualTo` ctType y
                   && ctSubtype x `starOrEqualTo` ctSubtype y
                   && all (hasParameter y) (ctParameters x)
    top = ContentType "*" "*" []

hasParameter :: ContentType -> (String, String) -> Bool
hasParameter t (k,v) = maybe False (==v) $ lookup k (ctParameters t)

--
-- ** Accept-Charset
--

{-
RFC 2616 14.2:

The special value "*", if present in the Accept-Charset field, matches
every character set (including ISO-8859-1) which is not mentioned
elsewhere in the Accept-Charset field. If no "*" is present in an
Accept-Charset field, then all character sets not explicitly mentioned
get a quality value of 0, except for ISO-8859-1, which gets a quality
value of 1 if not explicitly mentioned.

If no Accept-Charset header is present, the default is that any
character set is acceptable. If an Accept-Charset header is present,
and if the server cannot send a response which is acceptable according
to the Accept-Charset header, then the server SHOULD send an error
response with the 406 (not acceptable) status code, though the sending
of an unacceptable response is also allowed.
-}

newtype Charset = Charset String
    deriving (Show)

instance Eq Charset where
    Charset x == Charset y = caseInsensitiveEq x y

instance Ord Charset where
    Charset x `compare` Charset y = caseInsensitiveCompare x y

instance HeaderValue Charset where
    parseHeaderValue = fmap Charset $ many ws1 >> lexeme p_token
    prettyHeaderValue (Charset s) = s

instance Acceptable Charset where
    Charset x `includes` Charset y = starOrEqualTo x y
    top = Charset "*"

--
-- ** Accept-Encoding
--

{-
RFC 2616, section 14.3
-}

newtype ContentEncoding = ContentEncoding String
    deriving (Show)

instance Eq ContentEncoding where
    ContentEncoding x == ContentEncoding y = caseInsensitiveEq x y

instance Ord ContentEncoding where
    ContentEncoding x `compare` ContentEncoding y = caseInsensitiveCompare x y

instance HeaderValue ContentEncoding where
    parseHeaderValue = fmap ContentEncoding $ many ws1 >> lexeme p_token
    prettyHeaderValue (ContentEncoding s) = s

instance Acceptable ContentEncoding where
    ContentEncoding x `includes` ContentEncoding y = starOrEqualTo x y
    top = ContentEncoding "*"

--
-- ** Accept-Language
--

newtype Language = Language String
    deriving (Show)

instance Eq Language where
    Language x == Language y = caseInsensitiveEq x y

instance Ord Language where
    Language x `compare` Language y = caseInsensitiveCompare x y

instance HeaderValue Language where
    parseHeaderValue = fmap Language $ many ws1 >> lexeme p_token
    prettyHeaderValue (Language s) = s

{-
RFC 2616 14.4

A language-range matches a language-tag if it exactly equals the tag,
or if it exactly equals a prefix of the tag such that the first tag
character following the prefix is "-". The special range "*", if
present in the Accept-Language field, matches every tag not matched by
any other range present in the Accept-Language field.
-}
instance Acceptable Language where
    Language x `includes` Language y =
        x == "*" || x == y || (x `isPrefixOf` y && "-" `isPrefixOf` drop (length x) y)
    top = Language "*"