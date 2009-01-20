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
module OpenMath.Request (Request(..), getTerm, getContextTerm, getPrefix, pRequest, ppRequest) where

import Common.Utils
import Common.Context
import Common.Strategy hiding (fail)
import Control.Monad
import OpenMath.StrategyTable
import OpenMath.Object
import OpenMath.Conversion
import Service.XML
import Data.Char
import Data.Maybe

------------------------------------------------------------------------
-- Data type for requests

data Request = Request 
   { req_Strategy :: StrategyID 
   , req_Location :: StrategyLocation
   , req_Term     :: OMOBJ
   , req_Context  :: Maybe String
   , req_Answer   :: Maybe OMOBJ
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
getTerm :: IsOMOBJ a => Request -> Maybe a
getTerm = fromOMOBJ . req_Term
   
getContextTerm :: IsOMOBJ a => Request -> Maybe (Context a)
getContextTerm req = do
   a <- getTerm req
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

optional :: Either String a -> Either String (Maybe a)
optional = Right . either (const Nothing) Just

----------------------------
-- Pretty-printer for requests

ppRequest :: Request -> String
ppRequest = showXML . requestToXML 

requestToXML :: Request -> XML
requestToXML req = makeXML "request" $ do
   "service" .=. "mathdox"
   element "strategy" (text (req_Strategy req))
   element "location" (text (show (req_Location req)))
   element "term"     (builder (omobj2xml (req_Term req)))
   when (isJust (req_Context req)) $
      element "context" (text (fromJust (req_Context req)))
   when (isJust (req_Answer req)) $
      element "answer" (builder (omobj2xml (fromJust (req_Answer req))))
   
xmlToRequest :: XML -> Either String Request
xmlToRequest xml = do
   unless (name xml == "request") $
      fail "XML document is not a request" 
   
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

extractString :: String -> XML -> Either String String
extractString s xml = liftM getData (findChild s xml)

extractLocation :: String -> XML -> Either String StrategyLocation
extractLocation s xml = do
   c <- findChild s xml
   case reads (getData c) of
      [(n, xs)] | all isSpace xs -> return n
      _                          -> fail "invalid location"

extractExpr :: String -> XML -> Either String OMOBJ
extractExpr n xml = do
   case findChild n xml of 
      Just expr -> 
         case children expr of 
            [this] -> xml2omobj this
            _ -> fail $ "error in " ++ show (n, xml)
      _ -> fail $ "error in " ++ show (n, xml)