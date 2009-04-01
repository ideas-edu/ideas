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

import Common.Utils (Some(..), mapLeft)
import Common.Context
import Common.Exercise
import Common.Strategy hiding (not, fail)
import Common.Transformation hiding (name)
import qualified Common.Transformation as Rule
import Control.Monad
import OpenMath.Object
import Service.XML
import Service.ExerciseList
import Service.Revision (version)
import Service.ServiceList
import OpenMath.LAServer
import OpenMath.Reply
import OpenMath.Interactive (respondHTML)
import OpenMath.Conversion
import Service.TypedAbstractService hiding (exercise)
import Data.Maybe
import Data.Char
import System.IO.Unsafe

processXML :: Maybe String -> String -> IO (String, String)
processXML htmlMode input = 
   case (parseXML input, htmlMode) of
      (Left err, _) -> 
         fail err
      (Right _, Just self) ->           
         return (respondHTML self input, "text/html")
      (Right xml, _) -> do 
         out <- xmlRequestHandler xml
         return (showXML out, "application/xml")
         
xmlRequestHandler :: XML -> IO XML
xmlRequestHandler xml = do
   unless (name xml == "request") $
      fail "expected xml tag request"
   s    <- findAttribute "service" xml
   code <- extractExerciseCode xml
   conv <- liftM openMathConverter (getOpenMathExercise code) 
              `mplus`
           liftM xmlConverter (getExercise code)
   serviceXML (map toLower s) conv xml

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

serviceXML :: String -> Some (Converter XML) -> XML -> IO XML
serviceXML s (Some conv) request = 
   case getService s of
      Just service -> 
         case execute service conv request of
            Left  err -> return (resultError err)
            Right xml -> return xml
      Nothing  
         | s == "mathdox" -> do
              req <- fromXML request
              return $ replyToXML $ laServer req
         | otherwise ->  
              fail $ "Invalid request: unknown service " ++ show s

------------------------------------------------------------
-- Mixing abstract syntax (OpenMath format) and concrete syntax (string)

xmlConverter :: Some Exercise -> Some (Converter XML)
xmlConverter (Some ex) = Some $ Converter
   { exercise = ex
   , toTerm   = xmlRead
   , fromTerm = xmlBuild
   , toType   = toArgument ex xmlRead
   , fromType = \a b -> resultOk (fromResult xmlBuild a b)
   }
 where
   xmlBuild = makeXML "expr" . text . prettyPrinter ex
   xmlRead xml = do
                guard (name xml == "expr")
                let input = getData xml
                mapLeft show (parser ex input)
   
openMathConverter :: OpenMathExercise -> Some (Converter XML)
openMathConverter (OMEX ex) = Some $ Converter
   { exercise = ex
   , toTerm   = xmlRead
   , fromTerm = xmlBuild
   , toType   = toArgument ex xmlRead
   , fromType = \a b -> resultOk (fromResult xmlBuild a b)
   }
 where
   xmlRead xml = do 
      omobj <- xml2omobj xml
      case fromOMOBJ omobj of
         Just a  -> return a
         Nothing -> fail "Unknown OpenMath object"
   xmlBuild = toXML . toOMOBJ

toArgument :: Exercise a -> (XML -> Either String a) -> ServiceType a t -> XML -> Either String t 
toArgument ex f serviceType xml = 
   case serviceType of
      PairType t1 t2 -> do
         r1 <- toArgument ex f t1 xml
         r2 <- toArgument ex f t2 xml
         return (r1, r2)
      TripleType t1 t2 t3 -> do
         r1 <- toArgument ex f t1 xml
         r2 <- toArgument ex f t2 xml
         r3 <- toArgument ex f t3 xml
         return (r1, r2, r3)
      StateType -> 
         xml2State ex f xml
      LocationType -> 
         liftM (read . getData) (findChild "location" xml)
      RuleType ->
         liftM (fromJust . getRule ex . getData) (findChild "ruleid" xml)
      ExerciseType -> 
         return ex
      _ -> 
         fail "toArgument: unknown argument type"
          
fromResult :: (a -> XML) -> ServiceType a t -> t -> XMLBuilder
fromResult f serviceType tv = 
   case serviceType of
      ListType t1 -> 
         element "list" (mapM_ (\x -> element "elem" (fromResult f t1 x)) tv)
      PairType t1 t2 -> do
         let (a, b) = tv 
         fromResult f t1 a
         fromResult f t2 b
      TripleType t1 t2 t3 -> do 
         let (a, b, c) = tv 
         fromResult f t1 a
         fromResult f t2 b
         fromResult f t3 c
      ElemType t1 ->
         element "elem" (fromResult f t1 tv) -- quick fix  
      IOType t1 -> 
         fromResult f t1 (unsafePerformIO tv) -- quick fix                                                          
      RuleType -> 
         "ruleid" .=. Rule.name tv
      TermType -> 
         builder $ f $ fromContext tv
      LocationType -> 
         text (show tv)
      BoolType -> 
         text (show tv)
      IntType -> 
         text (show tv)
      StateType -> 
         builder $ state2xml f tv
      _ -> 
         fail "fromResult: unknown result type"
      
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

xml2State :: Exercise a -> (XML -> Either String a) -> XML -> Either String (State a)
xml2State ex f top = do
   xml <- findChild "state" top
   unless (name xml == "state") (fail "expected a state tag")
   sp   <- liftM getData (findChild "prefix" xml)
   sc   <- return $ maybe "" getData $ findChild "context" xml
   x    <- findChild "OMOBJ" xml
   expr <- f x
   let state  = State ex (Just (makePrefix (read sp) $ strategy ex)) term
       contxt = fromMaybe (error $ "invalid context" ++ show sc) $ parseContext sc
       term   = fmap (const expr) contxt
   return state
   
state2xml :: (a -> XML) -> State a -> XML
state2xml tBuild state = makeXML "state" $ do
   element "prefix"  (text $ maybe "[]" show (prefix state))
   element "context" (text $ showContext (context state))
   builder $ tBuild (term state)