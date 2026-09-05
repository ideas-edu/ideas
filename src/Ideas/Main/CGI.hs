{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-deprecations #-}
-- Original source: https://hackage.haskell.org/package/wai-extra-3.0.20.0/docs/Network-Wai-Handler-CGI.html
module Ideas.Main.CGI (run) where

import Blaze.ByteString.Builder (fromByteString, toLazyByteString, flush)
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import Control.Arrow ((***))
import Control.Monad (unless, void)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Char (toLower)
import Data.Function (fix)
import Data.IORef
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (Status (..), hRange, hContentType, hContentLength)
import Network.Socket (getAddrInfo, addrAddress)
import Network.Wai
import Network.Wai.Internal
import System.Environment (getEnvironment)
import System.IO (Handle)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import qualified Data.Streaming.ByteString.Builder as Blaze
import qualified Data.String as String
import qualified Network.HTTP.Types as H
import qualified System.IO

safeRead :: Read a => a -> String -> a
safeRead d s =
  case reads s of
    ((x, _):_) -> x
    [] -> d

lookup' :: String -> [(String, String)] -> String
lookup' key pairs = fromMaybe "" $ lookup key pairs

-- | Run an application using CGI.
run :: Application -> IO ()
run app = do
    vars <- getEnvironment
    let input = requestBodyHandle System.IO.stdin
        output = B.hPut System.IO.stdout
    runGeneric vars input output Nothing app

-- | A generic CGI helper, which allows other backends (FastCGI and SCGI) to
-- use the same code as CGI. Most users will not need this function, and can
-- stick with 'run' or 'runSendfile'.
runGeneric
     :: [(String, String)] -- ^ all variables
     -> (Int -> IO (IO B.ByteString)) -- ^ responseBody of input
     -> (B.ByteString -> IO ()) -- ^ destination for output
     -> Maybe B.ByteString -- ^ does the server support the X-Sendfile header?
     -> Application
     -> IO ()
runGeneric vars inputH outputH xsendfile app = do
    let rmethod = B.pack $ lookup' "REQUEST_METHOD" vars
        pinfo = lookup' "PATH_INFO" vars
        qstring = lookup' "QUERY_STRING" vars
        contentLength = safeRead 0 $ lookup' "CONTENT_LENGTH" vars
        remoteHost' =
            let s = fromMaybe "" (lookup "REMOTE_HOST" vars)
            in fromMaybe s (lookup "REMOTE_ADDR" vars)

        isSecure' =
            case map toLower $ lookup' "SERVER_PROTOCOL" vars of
                "https" -> True
                _ -> False
    addrs <- getAddrInfo Nothing (Just remoteHost') Nothing
    requestBody' <- inputH contentLength
    let addr =
            case addrs of
                a:_ -> addrAddress a
                [] -> error $ "Invalid REMOTE_ADDR or REMOTE_HOST: " ++ remoteHost'
        reqHeaders = map (cleanupVarName *** B.pack) vars
        env = Request
            { requestMethod = rmethod
            , rawPathInfo = B.pack pinfo
            , pathInfo = H.decodePathSegments $ B.pack pinfo
            , rawQueryString = B.pack qstring
            , queryString = H.parseQuery $ B.pack qstring
            , requestHeaders = reqHeaders
            , isSecure = isSecure'
            , remoteHost = addr
            , httpVersion = H.http11 -- FIXME
            , requestBody = requestBody'
            , vault = mempty
            , requestBodyLength = KnownLength $ fromIntegral contentLength
            , requestHeaderHost = lookup "host" reqHeaders
            , requestHeaderRange = lookup hRange reqHeaders
            , requestHeaderReferer = lookup "referer" reqHeaders
            , requestHeaderUserAgent = lookup "user-agent" reqHeaders
            }
    void $ app env $ \res ->
        case (xsendfile, res) of
            (Just sf, ResponseFile s hs fp Nothing) -> do
                mapM_ outputH $ L.toChunks $ toLazyByteString $ sfBuilder s hs sf fp
                return ResponseReceived
            _ -> do
                let (s, hs, wb) = responseToStream res
                (blazeRecv, blazeFinish) <- Blaze.newByteStringBuilderRecv Blaze.defaultStrategy
                wb $ \b -> do
                    let sendBuilder builder = do
                            popper <- blazeRecv builder
                            fix $ \loop -> do
                                bs <- popper
                                unless (B.null bs) $ do
                                    outputH bs
                                    loop
                    sendBuilder $ headers s hs `mappend` fromChar '\n'
                    b sendBuilder (sendBuilder flush)
                blazeFinish >>= maybe (return ()) outputH
                return ResponseReceived
  where
    headers s hs = mconcat (map header $ status s : map header' (fixHeaders hs))
    status (Status i m) = (fromByteString "Status", mconcat
        [ fromString $ show i
        , fromChar ' '
        , fromByteString m
        ])
    header' (x, y) = (fromByteString $ CI.original x, fromByteString y)
    header (x, y) = mconcat
        [ x
        , fromByteString ": "
        , y
        , fromChar '\n'
        ]
    sfBuilder s hs sf fp = mconcat
        [ headers s hs
        , header (fromByteString sf, fromString fp)
        , fromChar '\n'
        , fromByteString sf
        , fromByteString " not supported"
        ]
    fixHeaders h =
        case lookup hContentType h of
            Nothing -> (hContentType, "text/html; charset=utf-8") : h
            Just _ -> h

cleanupVarName :: String -> CI.CI B.ByteString
cleanupVarName "CONTENT_TYPE" = hContentType
cleanupVarName "CONTENT_LENGTH" = hContentLength
cleanupVarName "SCRIPT_NAME" = "CGI-Script-Name"
cleanupVarName s =
    case s of
        'H':'T':'T':'P':'_':a:as -> String.fromString $ a : helper' as
        _ -> String.fromString s -- FIXME remove?
  where
    helper' ('_':x:rest) = '-' : x : helper' rest
    helper' (x:rest) = toLower x : helper' rest
    helper' [] = []

requestBodyHandle :: Handle -> Int -> IO (IO B.ByteString)
requestBodyHandle h = requestBodyFunc $ \i -> do
    bs <- B.hGet h i
    return $ if B.null bs then Nothing else Just bs

requestBodyFunc :: (Int -> IO (Maybe B.ByteString)) -> Int -> IO (IO B.ByteString)
requestBodyFunc get count0 = do
    ref <- newIORef count0
    return $ do
        count <- readIORef ref
        if count <= 0
            then return B.empty
            else do
                mbs <- get $ min count defaultChunkSize
                writeIORef ref $ count - maybe 0 B.length mbs
                return $ fromMaybe B.empty mbs