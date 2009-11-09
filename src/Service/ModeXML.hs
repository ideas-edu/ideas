-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Services using XML notation
--
-----------------------------------------------------------------------------
module Service.ModeXML (processXML) where

import Common.Context
import Common.Exercise
import Common.Strategy hiding (not, fail)
import Common.Transformation hiding (name, defaultArgument)
import Common.Utils (Some(..))
import Control.Monad
import Data.Char
import Data.Maybe
import Service.ExerciseList
import Service.ProblemDecomposition
import Service.Request
import Service.Revision (version)
import Service.ServiceList 
import Service.TypedAbstractService hiding (exercise)
import Service.Types hiding (State)
import Text.OpenMath.Object
import Text.OpenMath.Reply (replyToXML)
import Text.OpenMath.Request (xmlToRequest)
import Text.XML
import qualified Common.Transformation as Rule
import qualified Service.Types as Tp

processXML :: String -> IO (Request, String, String)
processXML input = 
   either fail return $ do
      xml <- parseXML input
      req <- xmlRequest xml
      out <- xmlRequestHandler xml
      return (req, showXML out, "application/xml") 

xmlRequest :: XML -> Either String Request
xmlRequest xml = do   
   unless (name xml == "request") $
      fail "expected xml tag request"
   srv  <- findAttribute "service" xml
   let code = extractExerciseCode xml
   enc  <- case findAttribute "encoding" xml of
              Just s  -> liftM Just (readEncoding s)
              Nothing -> return Nothing 
   return Request 
      { service    = srv
      , exerciseID = code
      , source     = findAttribute "source" xml
      , dataformat = XML
      , encoding   = enc
      }

xmlReply :: Request -> XML -> Either String XML
xmlReply request xml
   | service request == "mathdox" = do
        code <- maybe (fail "unknown exercise code") return (exerciseID request)
        Some pkg <- getPackage code
        (st, sloc, answer) <-  xmlToRequest xml (fromOpenMath pkg) (exercise pkg)
        return (replyToXML (toOpenMath pkg) (problemDecomposition st sloc answer))
--   | service request == "ping" = do
--        return (resultOk (return ()))
   | otherwise =
   case encoding request of
      Just StringEncoding -> do 
         code <- maybe (fail "unknown exercise code") return (exerciseID request)
         pkg  <- getPackage code
         case stringFormatConverter pkg of
            Some conv -> do
               srv <- getService (service request)
               res <- evalService conv srv xml 
               return (resultOk res)
      _ -> do 
         code <- maybe (fail "unknown exercise code") return (exerciseID request)
         ex <- getPackage code
         case openMathConverter ex of
            Some conv -> do
               srv <- getService (service request)
               res <- evalService conv srv xml 
               return (resultOk res)

xmlRequestHandler :: Monad m => XML -> m XML
xmlRequestHandler xml =
   case xmlRequest xml of 
      Left err -> return (resultError err)
      Right request ->  
         case xmlReply request xml of
            Left err     -> return (resultError err)
            Right result -> return result

extractExerciseCode :: Monad m => XML -> m ExerciseCode
extractExerciseCode xml =
   case liftM (break (== '.')) (findAttribute "exerciseid" xml) of
      Just (as, _:bs) -> return (makeCode as bs)
      Just (as, _)    -> maybe (fail "invalid code") return (readCode as)
      -- being backwards compatible with early MathDox
      Nothing ->
         case fmap getData (findChild "strategy" xml) of
            Just name -> 
               let s ~= t = f s == f t 
                   f = map toLower . filter isAlphaNum
               in case [ exerciseCode ex | Some ex <- exercises, name ~= description ex ] of
                     [code] -> return code
                     _ -> fail $ "Unknown strategy name " ++ show name
            _ -> fail "no exerciseid attribute, nor a known strategy element" 

resultOk :: XMLBuilder -> XML
resultOk body = makeXML "reply" $ do 
   "result"  .=. "ok"
   "version" .=. version
   body

resultError :: String -> XML
resultError txt = makeXML "reply" $ do 
   "result"  .=. "error"
   "version" .=. version
   element "message" (text txt)

------------------------------------------------------------
-- Mixing abstract syntax (OpenMath format) and concrete syntax (string)

stringFormatConverter :: Some ExercisePackage -> Some (Evaluator (Either String) XML XMLBuilder)
stringFormatConverter (Some pkg) = 
   Some $ Evaluator (xmlEncoder f ex) (xmlDecoder g pkg)
 where
   ex = exercise pkg
   f  = return . element "expr" . text . prettyPrinter ex
   g xml = do
      xml <- findChild "expr" xml -- quick fix
      -- guard (name xml == "expr")
      let input = getData xml
      either (fail . show) return (parser ex input)
        
openMathConverter :: Some ExercisePackage -> Some (Evaluator (Either String) XML XMLBuilder)
openMathConverter (Some pkg) =
   Some $ Evaluator (xmlEncoder f ex) (xmlDecoder g pkg)
 where
   ex = exercise pkg
   f = return . builder . toXML . toOpenMath pkg
   g xml = do
      xob   <- findChild "OMOBJ" xml
      omobj <- xml2omobj xob
      case fromOpenMath pkg omobj of
         Just a  -> return a
         Nothing -> fail "Unknown OpenMath object"

xmlEncoder :: Monad m => (a -> m XMLBuilder) -> Exercise a -> Encoder m XMLBuilder a
xmlEncoder f ex = Encoder
   { encodeType  = encode (xmlEncoder f ex)
   , encodeTerm  = f
   , encodeTuple = sequence_
   }
 where
   encode :: Monad m => Encoder m XMLBuilder a -> Type a t -> t -> m XMLBuilder
   encode enc serviceType =
      case serviceType of
         Tp.List t1  -> \xs -> do
            bs <- mapM (encode enc t1) xs
            let b = mapM_ (element "elem") bs
            return (element "list" b)
         Tp.Elem t1  -> liftM (element "elem") . encode enc t1
         Tp.Tag s t1 -> liftM (element s) . encode enc t1  -- quick fix
         Tp.Rule     -> return . ("ruleid" .=.) . Rule.name
         Tp.Term     -> encodeTerm enc
         Tp.Context  -> encodeContext (encodeTerm enc)
         Tp.Location -> return . text . show
         Tp.Bool     -> return . text . show
         Tp.Int      -> return . text . show
         Tp.State    -> encodeState (encodeTerm enc)
         _           -> encodeDefault enc serviceType

xmlDecoder :: MonadPlus m => (XML -> m a) -> ExercisePackage a -> Decoder m XML a
xmlDecoder f pkg = Decoder
   { decodeType     = decode (xmlDecoder f pkg)
   , decodeTerm     = f
   , decoderPackage = pkg
   }
 where
   decode :: MonadPlus m => Decoder m XML a -> Type a t -> XML -> m (t, XML)
   decode dec serviceType = 
      case serviceType of
         Tp.State    -> decodeState (decoderExercise dec) (decodeTerm dec)
         Tp.Location -> leave $ liftM (read . getData) . findChild "location"
         Tp.Rule     -> leave $ fromMaybe (fail "unknown rule") . liftM (getRule (decoderExercise dec) . getData) . findChild "ruleid"
         Tp.Term     -> \xml -> decodeTerm dec xml >>= \a -> return (a, xml)
         _           -> decodeDefault dec serviceType
         
   leave :: Monad m => (XML -> m a) -> XML -> m (a, XML)
   leave f xml = liftM (\a -> (a, xml)) (f xml)
         
decodeState :: Monad m => Exercise a -> (XML -> m a) -> XML -> m (State a, XML)
decodeState ex f top = do
   xml <- findChild "state" top
   unless (name xml == "state") (fail "expected a state tag")
   let sp = maybe "[]" getData (findChild "prefix" xml)
   expr <- f xml
   env  <- decodeEnvironment xml
   let state  = State ex (Just (makePrefix (read sp) $ strategy ex)) term
       term   = makeContext env expr
   return (state, top)

decodeEnvironment :: Monad m => XML -> m Environment
decodeEnvironment xml =
   case findChild "context" xml of 
      Just this -> foldM add emptyEnv (children this)
      Nothing   -> return emptyEnv
 where
   add env item = do 
      unless (name item == "item") $ 
         fail $ "expecting item tag, found " ++ name item
      name  <- findAttribute "name"  item
      case findChild "OMOBJ" item of
         -- OpenMath object found inside item tag
         Just this -> do
            case xml2omobj this of
               Left err -> fail err
               Right omobj -> 
                  return (storeEnv name omobj env)
         -- Simple value in attribute
         Nothing -> do
            value <- findAttribute "value" item
            return (storeEnv name value env)

encodeState :: Monad m => (a -> m XMLBuilder) -> State a -> m XMLBuilder
encodeState f state = f (term state) >>= \body -> return $
   element "state" $ do
      element "prefix"  (text $ maybe "[]" show (prefix state))
      encodeEnvironment (getEnvironment (context state))
      body

encodeEnvironment :: Environment -> XMLBuilder
encodeEnvironment env
   | nullEnv env = return ()
   | otherwise   = element "context" $ do
        forM_ (keysEnv env) $ \k -> do
           element "item" $ do 
              "name"  .=. k
              case lookupEnv k env of 
                 Just omobj -> builder  (omobj2xml omobj)
                 Nothing    -> "value" .=. fromMaybe "" (lookupEnv k env)
            
encodeContext :: Monad m => (a -> m XMLBuilder) -> Context a -> m XMLBuilder
encodeContext f ctx = do
   xml <- f (fromContext ctx)
   return (xml >> encodeEnvironment (getEnvironment ctx))