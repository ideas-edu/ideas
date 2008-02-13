module OpenMath.Request (Request(..), getContextTerm, pRequest, ppRequest) where

import Common.Context
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
   , req_Context  :: Maybe String
   , req_Answer   :: Maybe Expr
   }
 deriving Show
 
----------------------------
-- XML parser for requests

pRequest :: String -> Either String Request
pRequest input = do
   xml     <- parseXML input
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

-- smart extractor
getContextTerm :: IsExpr a => Request -> Maybe (Context a)
getContextTerm req = do
   a <- fromExpr (req_Term req)
   return (putInContext req a)

putInContext :: Request -> a -> Context a
putInContext req = fromMaybe inContext $ do
   s <- req_Context req
   c <- parseContext s
   return (flip fmap c . const)

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
   [ Tag "context" [] [Text $ fromJust $ req_Context req ]
   | isJust (req_Context req)
   ] ++
   [ Tag "answer" [] [exprToXML $ fromJust $ req_Answer req]
   | isJust (req_Answer req)
   ]