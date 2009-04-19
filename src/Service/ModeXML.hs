{-# OPTIONS -XRankNTypes #-}
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
import Service.Types (Evaluator(..), Type, encodeDefault, decodeDefault, Encoder(..), Decoder(..))
import Text.OpenMath.Conversion
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
   return $ Request 
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
        OMEX ex <- getOpenMathExercise code
        (st, sloc, answer) <- xmlToRequest xml ex
        return (replyToXML (problemDecomposition st sloc answer))
   | otherwise =
   case encoding request of
      Just StringEncoding -> do 
         code <- maybe (fail "unknown exercise code") return (exerciseID request)
         ex <- getExercise code
         case stringFormatConverter ex of
            Some conv -> do
               srv <- getService (service request)
               res <- evalService conv srv xml 
               return (resultOk res)
      _ -> do 
         code <- maybe (fail "unknown exercise code") return (exerciseID request)
         ex <- getOpenMathExercise code
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
   case findAttribute "exerciseid" xml >>= return . break (=='.') of
      Just (as, _:bs) -> return (makeCode as bs)
      Just (as, _)    -> resolveExerciseCode as
      -- being backwards compatible with early MathDox
      Nothing ->
         case fmap getData (findChild "strategy" xml) of
            Just name -> 
               let s ~= t = f s == f t 
                   f = map toLower . filter isAlphaNum
               in case findOpenMathExercises (\ex -> name ~= identifier ex) of 
                     [OMEX a] -> return (exerciseCode a)
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

stringFormatConverter :: Some Exercise -> Some (Evaluator (Either String) XML XMLBuilder)
stringFormatConverter (Some ex) = 
   Some $ Evaluator (xmlEncoder f ex) (xmlDecoder g ex)
 where
   f = return . element "expr" . text . prettyPrinter ex
   g xml = do
      guard (name xml == "expr")
      let input = getData xml
      either (fail . show) return (parser ex input)
                
openMathConverter :: OpenMathExercise -> Some (Evaluator (Either String) XML XMLBuilder)
openMathConverter (OMEX ex) = 
   Some $ Evaluator (xmlEncoder f ex) (xmlDecoder g ex)
 where
   f = return . builder . toXML . toOMOBJ
   g xml = do 
      omobj <- xml2omobj xml
      case fromOMOBJ omobj of
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
         Tp.Term     -> encodeTerm enc . fromContext
         Tp.Location -> return . text . show
         Tp.Bool     -> return . text . show
         Tp.Int      -> return . text . show
         Tp.State    -> encodeState (encodeTerm enc)
         _           -> encodeDefault enc serviceType

xmlDecoder :: MonadPlus m => (XML -> m a) -> Exercise a -> Decoder m XML a
xmlDecoder f ex = Decoder
   { decodeType      = decode (xmlDecoder f ex)
   , decodeTerm      = f
   , decoderExercise = ex
   }
 where
   decode :: MonadPlus m => Decoder m XML a -> Type a t -> XML -> m (t, XML)
   decode dec serviceType = 
      case serviceType of
         Tp.State    -> decodeState (decoderExercise dec) (decodeTerm dec)
         Tp.Location -> leave $ liftM (read . getData) . findChild "location"
         Tp.Rule     -> leave $ liftM (fromJust . getRule (decoderExercise dec) . getData) . findChild "ruleid"
         Tp.Exercise -> leave $ const (return (decoderExercise dec))
         _           -> decodeDefault dec serviceType
         
   leave :: Monad m => (XML -> m a) -> XML -> m (a, XML)
   leave f xml = liftM (\a -> (a, xml)) (f xml)
         
decodeState :: Monad m => Exercise a -> (XML -> m a) -> XML -> m (State a, XML)
decodeState ex f top = do
   xml <- findChild "state" top
   unless (name xml == "state") (fail "expected a state tag")
   sp   <- liftM getData (findChild "prefix" xml)
   let sc = maybe "" getData (findChild "context" xml)
   x    <- findChild "OMOBJ" xml
   expr <- f x
   contxt <- maybe (fail $ "invalid context" ++ show sc) return (parseContext sc)
   let state  = State ex (Just (makePrefix (read sp) $ strategy ex)) term
       term   = fmap (const expr) contxt
   return (state, top)

encodeState :: Monad m => (a -> m XMLBuilder) -> State a -> m XMLBuilder
encodeState f state = do
   b <- f (term state)
   return $ element "state" $ do
      element "prefix"  (text $ maybe "[]" show (prefix state))
      element "context" (text $ showContext (context state))
      b
   