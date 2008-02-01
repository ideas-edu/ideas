module OpenMath.Request (Request(..), pRequest, ppRequest) where

import OpenMath.StrategyTable
import OpenMath.ObjectParser
import OpenMath.XML
import Data.Char
import Data.Maybe

------------------------------------------------------------------------
-- Data type for requests

data Request = Request 
   { req_Strategy :: StrategyID 
   , req_Location :: Location
   , req_Term     :: Expr
   , req_Answer   :: Maybe Expr
   }
 deriving Show
 
----------------------------
-- XML parser for requests

pRequest :: String -> Either String Request
pRequest input = do
   xml <- parseXML input
   isRequest xml
   sid    <- extractString "strategy" xml
   loc    <- optional (extractLocation "location" xml)
   term   <- extractExpr "term" xml
   answer <- optional (extractExpr "answer" xml)
   return $ Request 
      { req_Strategy = sid 
      , req_Location = fromMaybe [] loc 
      , req_Term     = term 
      , req_Answer   = answer
      }
   
isRequest :: XML -> Either String ()
isRequest (Tag "request" [] _) = return ()
isRequest _ = fail "XML document is not a request"

extractTextWith :: (String -> Either String a) -> String -> XML -> Either String a
extractTextWith f n xml = 
   case extract n xml of 
      Just [Text s] -> f s
      _             -> fail $ "error in " ++ n

extractString :: String -> XML -> Either String String
extractString = extractTextWith Right

extractLocation :: String -> XML -> Either String Location
extractLocation = extractTextWith $ \s -> 
   case reads s of
      [(n, xs)] | all isSpace xs -> return n
      _                          -> fail "invalid location"

extractExpr :: String -> XML -> Either String Expr
extractExpr n xml = do
   this <- case extract n xml of 
              Just [expr] -> return expr
              Nothing     -> fail $ "error in " ++ n
   xmlToExpr this

optional :: Either String a -> Either String (Maybe a)
optional = Right . either (const Nothing) Just

----------------------------
-- Pretty-printer for requests

ppRequest :: Request -> String
ppRequest req = showXML $ Tag "request" [] $
   [ Tag "strategy" [] [Text $ req_Strategy req]
   , Tag "location" [] [Text $ show $ req_Location req]
   , Tag "term"     [] [exprToXML $ req_Term req]
   ] ++ 
   [ Tag "answer" [] [exprToXML $ fromJust $ req_Answer req]
   | isJust (req_Answer req)
   ]