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
-----------------------------------------------------------------------------

module Ideas.Text.JSON.RPC
   ( jsonRPC, RPCHandler, RPCRequest(..), RPCResponse(..)
   ) where

import Control.Exception
import Ideas.Text.JSON.Data
import Ideas.Text.JSON.Class
import Ideas.Text.JSON.Decoder
import System.IO.Error

--------------------------------------------------------
-- JSON-RPC

data RPCRequest = Request
   { requestMethod :: String
   , requestParams :: JSON
   , requestId     :: JSON
   }

data RPCResponse = Response
   { responseResult :: JSON
   , responseError  :: JSON
   , responseId     :: JSON
   }

instance InJSON RPCRequest where
   toJSON req = Object
      [ ("method", String $ requestMethod req)
      , ("params", requestParams req)
      , ("id"    , requestId req)
      ]
   jsonDecoder = jObject $
      Request <$> jKey "method" jString 
              <*> (jKey "params" (jNext Right) <|> pure Null)
              <*> (jKey "id"     (jNext Right) <|> pure Null)

instance InJSON RPCResponse where
   toJSON resp = Object
      [ ("result", responseResult resp)
      , ("error" , responseError resp)
      , ("id"    , responseId resp)
      ]
   jsonDecoder = jObject $ 
      Response <$> jKey "result" (jNext Right)
               <*> jKey "error"  (jNext Right)
               <*> jKey "id"     (jNext Right)

instance Show RPCRequest where
   show = show . toJSON

instance Show RPCResponse where
   show = show . toJSON

okResponse :: JSON -> JSON -> RPCResponse
okResponse x y = Response
   { responseResult = x
   , responseError  = Null
   , responseId     = y
   }

errorResponse :: JSON -> JSON -> RPCResponse
errorResponse x y = Response
   { responseResult = Null
   , responseError  = x
   , responseId     = y
   }

--------------------------------------------------------
-- JSON-RPC over HTTP

type RPCHandler = String -> JSON -> IO JSON

jsonRPC :: JSON -> RPCHandler -> IO RPCResponse
jsonRPC input rpc =
   case fromJSON input of
      Nothing  -> return (errorResponse (String "Invalid request") Null)
      Just req -> do
         json <- rpc (requestMethod req) (requestParams req)
         return (okResponse json (requestId req))
       `catch` handler req
 where
   handler :: RPCRequest -> SomeException -> IO RPCResponse
   handler req e =
      let msg = maybe (show e) ioeGetErrorString (fromException e)
      in return $ errorResponse (toJSON msg) (requestId req)