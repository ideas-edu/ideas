-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.Compat
-- Copyright   :  (c) The University of Glasgow 2001
--                (c) Bjorn Bringert 2004-2006
--                (c) Ian Lynagh 2005
--                (c) Jeremy Shaw 2005
-- License     :  BSD-style
--
-- Maintainer  :  John Chee <cheecheeo@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Monad.State)
--
-- Compatibility functions for the old Network.CGI API.
--
-----------------------------------------------------------------------------

module Network.CGI.Compat (
    Html, wrapper, pwrapper, connectToCGIScript
  ) where

import Control.Concurrent (forkIO)
import Control.Exception as Exception (SomeException, throw, catch, finally)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)
import Network (PortID, Socket, listenOn, connectTo)
import Network.Socket as Socket (SockAddr(SockAddrInet), accept, socketToHandle)
import System.IO (Handle, hPutStrLn, stdin, stdout, hGetLine, hClose, IOMode(ReadWriteMode))
import qualified Data.Map as Map

import System.IO.Error (isEOFError)

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Text.XHtml (Html, renderHtml)

import Network.CGI.Protocol

{-# DEPRECATED wrapper, pwrapper, connectToCGIScript "Use the new interface." #-}

-- | Compatibility wrapper for the old CGI interface.
--   Output the output from a function from CGI environment and
--   input variables to an HTML document.
wrapper :: ([(String,String)] -> IO Html) -> IO ()
wrapper = run stdin stdout

-- | Compatibility wrapper for the old CGI interface.
--   Runs a simple CGI server.
--   Note: if using Windows, you might need to wrap 'Network.withSocketsDo' around main.
pwrapper :: PortID  -- ^ The port to run the server on.
         -> ([(String,String)] -> IO Html)
         -> IO ()
pwrapper pid f = do sock <- listenOn pid
                    acceptConnections fn sock
 where fn h = run h h f

acceptConnections :: (Handle -> IO ()) -> Socket -> IO ()
acceptConnections fn sock = do
  (h, SockAddrInet _ _) <- accept' sock
  _ <- forkIO (fn h `finally` (hClose h))
  acceptConnections fn sock

accept' :: Socket                 -- Listening Socket
       -> IO (Handle,SockAddr)        -- StdIO Handle for read/write
accept' sock = do
 (sock', addr) <- Socket.accept sock
 handle        <- socketToHandle sock' ReadWriteMode
 return (handle,addr)

run :: MonadIO m => Handle -> Handle -> ([(String,String)] -> IO Html) -> m ()
run inh outh f =
    do env <- getCGIVars
       hRunCGI env inh outh f'
  where f' req = do let vs = Map.toList (cgiVars req)
                        is = [ (n,BS.unpack (inputValue i)) | (n,i) <- cgiInputs req ]
                    html <- liftIO (f (vs++is))
                    return ([], CGIOutput $ BS.pack $ renderHtml html)

-- | Note: if using Windows, you might need to wrap 'Network.withSocketsDo' around main.
connectToCGIScript :: String -> PortID -> IO ()
connectToCGIScript host portId
     = do env <- getCGIVars
          input <- BS.hGetContents stdin
          let str = getRequestInput env input
          h <- connectTo host portId
                 `Exception.catch`
                   (\ e -> abort "Cannot connect to CGI daemon." e)
          BS.hPut h str >> hPutStrLn h ""
          (sendBack h `finally` hClose h)
               `Exception.catch` (\e -> unless (isEOFError e) (ioError e))

-- | Returns the query string, or the request body if it is
--   a POST request, or the empty string if there is an error.
getRequestInput :: [(String,String)] -- ^ CGI environment variables.
                -> ByteString            -- ^ Request body.
                -> ByteString            -- ^ Query string.
getRequestInput env req =
   case lookup "REQUEST_METHOD" env of
      Just "POST" -> takeInput env req
      _ -> maybe BS.empty BS.pack (lookup "QUERY_STRING" env)

abort :: String -> SomeException -> IO a
abort msg e =
    do putStrLn ("Content-type: text/html\n\n" ++
                   "<html><body>" ++ msg ++ "</body></html>")
       throw e

sendBack :: Handle -> IO ()
sendBack h = do s <- hGetLine h
                putStrLn s
                sendBack h