-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module OpenMath.Request (Request(..), getContextTerm, getPrefix, pRequest, ppRequest) where

import Common.Utils
import Common.Context
import Common.Strategy hiding (fail)
import OpenMath.StrategyTable
import OpenMath.ObjectParser
import Service.XML
import Data.Char
import Data.Maybe

------------------------------------------------------------------------
-- Data type for requests

data Request = Request 
   { req_Strategy :: StrategyID 
   , req_Location :: StrategyLocation
   , req_Term     :: Expr
   , req_Context  :: Maybe String
   , req_Answer   :: Maybe Expr
   }
 deriving Show
 
----------------------------
-- XML parser for requests

instance InXML Request where
   toXML   = requestToXML
   fromXML = either fail return . xmlToRequest
   
pRequest :: String -> Either String Request
pRequest input = parseXML input >>= xmlToRequest

-- smart extractor
getContextTerm :: IsExpr a => Request -> Maybe (Context a)
getContextTerm req = do
   a <- fromExpr (req_Term req)
   return (putInContext req a)

putInContext :: Request -> a -> Context a
putInContext req = fromMaybe inContext $ do
   s       <- req_Context req
   (_, s2) <- splitAtElem ';' s
   c       <- parseContext s2
   return (flip fmap c . const)

getPrefix :: Request -> LabeledStrategy a -> Prefix a
getPrefix req ls = fromMaybe (emptyPrefix ls) $ do
   s       <- req_Context req
   (s1, _) <- splitAtElem ';' s
   case reads s1 of
      [(is, xs)] | all isSpace xs -> return (makePrefix is ls)
      _ -> Nothing

isRequest :: XML -> Either String ()
isRequest (Tag "request" _ _) = return ()
isRequest _ = fail "XML document is not a request"

extractTextWith :: (String -> Either String a) -> String -> XML -> Either String a
extractTextWith f n xml = 
   case extract n xml of 
      Just [Text s] -> f s
      _             -> fail $ "error in " ++ n

extractString :: String -> XML -> Either String String
extractString = extractTextWith Right

extractLocation :: String -> XML -> Either String StrategyLocation
extractLocation = extractTextWith $ \s -> 
   case reads s of
      [(n, xs)] | all isSpace xs -> return n
      _                          -> fail "invalid location"

extractExpr :: String -> XML -> Either String Expr
extractExpr n xml = do
   this <- case extract n xml of 
              Just [expr] -> return expr
              _           -> fail $ "error in " ++ n
   xmlToExpr this

optional :: Either String a -> Either String (Maybe a)
optional = Right . either (const Nothing) Just

----------------------------
-- Pretty-printer for requests

ppRequest :: Request -> String
ppRequest = showXML . requestToXML 

requestToXML :: Request -> XML
requestToXML req = Tag "request" [("service", "mathdox")] $
   [ Tag "strategy" [] [Text $ req_Strategy req]
   , Tag "location" [] [Text $ show $ req_Location req]
   , Tag "term"     [] [exprToXML $ req_Term req]
   ] ++ 
   [ Tag "context" [] [Text $ fromJust $ req_Context req ]
   | isJust (req_Context req)
   ] ++
   [ Tag "answer" [] [exprToXML $ fromJust $ req_Answer req]
   | isJust (req_Answer req)
   ]
   
xmlToRequest :: XML -> Either String Request
xmlToRequest xml = do
   isRequest xml
   sid     <- extractString "strategy" xml
   loc     <- optional (extractLocation "location" xml)
   term    <- extractExpr "term" xml
   context <- optional (extractString "context" xml)
   answer  <- optional (extractExpr "answer" xml)
   return $ Request 
      { req_Strategy = sid 
      , req_Location = fromMaybe [] loc
      , req_Term     = term 
      , req_Context  = context
      , req_Answer   = answer
      }