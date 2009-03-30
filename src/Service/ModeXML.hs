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
{-# OPTIONS -fglasgow-exts #-}
module Service.ModeXML (processXML) where

import Common.Utils (Some(..), safeHead)
import Common.Context
import Common.Exercise
import Common.Strategy hiding (not, fail)
import Common.Transformation hiding (name)
import qualified Common.Transformation as Transformation
import Control.Monad
import OpenMath.Object
import Service.XML
import Service.ExerciseList
import Service.Revision (version)
import OpenMath.LAServer
import OpenMath.Reply
import OpenMath.Interactive (respondHTML)
import OpenMath.Conversion
import qualified Service.TypedAbstractService as TAS
import Data.Maybe
import Data.Char

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
   s  <- findAttribute "service" xml
   ec <- extractExerciseCode xml
   triple <- omTriple ec `mplus` concTriple ec -- first try math exercises
   serviceXML (map toLower s) triple xml

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

serviceXML :: String -> Some Triple -> XML -> IO XML
serviceXML s (Some triple@(Triple tEx tRead tBuild)) request
   | s == "derivation" = do
        state <- xml2State triple request
        let list   = TAS.derivation state
            f (r, a) = element "elem" $ do 
               "ruleid" .=. show r
               tBuild (fromContext a)
        return $ resultOk $ element "list" (mapM_ f list)
   | s == "allfirsts" = do
        state <- xml2State triple request
        let list   = TAS.allfirsts state
            f (r, _, a) = element "elem" $ do
               "ruleid" .=. show r
               builder (state2xml triple a)
        return $ resultOk $ element "list" (mapM_ f list)
   | s == "onefirst" = do
        state <-  xml2State triple request
        let this = TAS.onefirst state
            f (r, _, a) = element "elem" $ do
               "ruleid" .=. show r
               builder (state2xml triple a)
        return $ resultOk $ f this
   | s == "ready" = do
        state <-  xml2State triple request
        let a = TAS.ready state
        return $ resultOk $ text $ show a
   | s == "stepsremaining" = do
        state <-  xml2State triple request
        let a = TAS.stepsremaining state
        return $ resultOk $ text $ show a
   | s == "applicable" = do
        state <-  xml2State triple request
        let loc   = maybe (error "no location") getData (findChild "location" request)         
            list  = TAS.applicable (read loc) state
            f r   = element "elem" ("ruleid" .=. show r)
        return $ resultOk $ element "list" (mapM_ f list)
   | s == "apply" = do
        state <-  xml2State triple request
        let rid   = maybe (error "no ruleid") getData (findChild "ruleid" request)
            rule  = fromMaybe (error "invalid ruleid") $ safeHead $ filter p (ruleset (TAS.exercise state))
            p     = (==rid) . Transformation.name
            loc   = maybe (error "no location") getData (findChild "location" request)
            this  = TAS.apply rule (read loc) state
        return $ resultOk $ builder $ state2xml triple this
   | s == "generate" = do
        let diff  = fromMaybe (error "no difficulty") $ findAttribute "difficulty" request 
        state <- TAS.generate tEx (read diff)
        return $ resultOk $ builder $ state2xml triple state
   | s == "mathdox" = do
        req <- fromXML request
        return $ replyToXML $ laServer req
serviceXML s _ _ = fail $ "Invalid request: unknown service " ++ show s

xml2State :: Monad m => Triple a -> XML -> m (TAS.State a)
xml2State (Triple tEx tRead _) top = do
   xml <- findChild "state" top
   unless (name xml == "state") (fail "expected a state tag")
   sp   <- liftM getData (findChild "prefix" xml)
   sc   <- return $ maybe "" getData $ findChild "context" xml
   x    <- findChild "OMOBJ" xml
   expr <- maybe (fail "reading") return (tRead x)
   let state  = TAS.State tEx (Just (makePrefix (read sp) $ strategy tEx)) term
       contxt = fromMaybe (error $ "invalid context" ++ show sc) $ parseContext sc
       term   = fmap (const expr) contxt
   return state

state2xml :: Triple a -> TAS.State a -> XML
state2xml (Triple _ _ tBuild) state = makeXML "state" $ do
   element "prefix"  (text $ maybe "[]" show (TAS.prefix state))
   element "context" (text $ showContext (TAS.context state))
   tBuild (TAS.term state)

resultOk :: XMLBuilder -> XML
resultOk body = makeXML "reply" $ do 
   "result"  .=. "ok"
   "version" .=. version
   body

------------------------------------------------------------
-- Mixing abstract syntax (OpenMath format) and concrete syntax (string)

data Triple a = Triple (Exercise a) (XML -> Maybe a) (a -> XMLBuilder)

omTriple :: ExerciseCode -> IO (Some Triple)
omTriple code = 
   case getOpenMathExercise code of
      Just (OMEX a) -> return $ Some $ 
         Triple a (either fail fromOMOBJ . xml2omobj) (builder . toXML . toOMOBJ)
      _ -> fail "exercise code not found"

concTriple :: ExerciseCode -> IO (Some Triple)
concTriple code = 
   case Service.ExerciseList.getExercise code of
      Just (Some a) -> return $ Some $ 
         let reader xml = do
                guard (name xml == "expr")
                let input = getData xml
                either (fail . show) return (parser a input)
         in Triple a reader (text . prettyPrinter a)
      _ -> fail "exercise code not found"