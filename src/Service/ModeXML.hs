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

import Common.Context
import Common.Exercise
import Common.Strategy hiding (not, fail)
import Common.Transformation
import Common.Utils (safeHead)
import Domain.Derivative
import OpenMath.Object
import Service.XML
import Service.AbstractService (getExercise, SomeExercise(..))
import OpenMath.LAServer
import OpenMath.Reply
import OpenMath.Interactive (respondHTML)
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
xmlRequestHandler xml@(Tag "request" attrs _) = 
   case lookup "service" attrs of 
      Just s -> serviceXML (map toLower s) attrs xml
      _      -> fail "attribute service not found" 
xmlRequestHandler _ = fail "expected xml tag request"

-- temporary
serviceXML :: String -> AttrList -> XML -> IO XML
serviceXML s attrs request
   | s == "derivation" = do
        X state <- getState request
        let list   = TAS.derivation state
            f (r, a) = Tag "elem" [("ruleid", show r)] [toXML $ fromContext a]
        return $ resultOk $ Tag "list" [] (map f list)
   | s == "allfirsts" = do
        X state <- getState request
        let list   = TAS.allfirsts state
            f (r, _, a) = Tag "elem" [("ruleid", show r)] [state2xml a]
        return $ resultOk $ Tag "list" [] (map f list)
   | s == "onefirst" = do
        X state <- getState request
        let this   = TAS.onefirst state
            f (r, _, a) = Tag "elem" [("ruleid", show r)] [state2xml a]
        return $ resultOk $ f this
   | s == "ready" = do
        X state <- getState request
        let a = TAS.ready state
        return $ resultOk $ toXML a
   | s == "stepsremaining" = do
        X state <- getState request
        let a = TAS.stepsremaining state
        return $ resultOk $ toXML a
   | s == "applicable" = do
        X state <- getState request
        let loc   = fromMaybe (error "no location") (extractText "location" request)         
            list  = TAS.applicable (read loc) state
            f r   = Tag "elem" [("ruleid", show r)] []
        return $ resultOk $ Tag "list" [] (map f list)
   | s == "apply" = do
        X state <- getState request
        let rid   = fromMaybe (error "no ruleid") (extractText "ruleid" request)
            rule  = fromMaybe (error "invalid ruleid") $ safeHead $ filter p (ruleset (TAS.exercise state))
            p     = (==rid) . name
            loc   = fromMaybe (error "no location") (extractText "location" request)
            this  = TAS.apply rule (read loc) state
        return $ resultOk $ state2xml this
   | s == "generate" =
        case getExercise "Derivative" of  -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!
           SE _ -> do
              let diff  = fromMaybe (error "no difficulty") $ lookup "difficulty" attrs 
              state <- TAS.generate derivativeExercise (read diff) -- !!!!!!!!!!!!!!!!!!!!!!!!!!!
              return $ resultOk $ state2xml state
   | s == "mathdox" = do
        req <- fromXML request
        return $ replyToXML $ laServer req
        
serviceXML s _ _ = fail $ "Invalid request: unknown service " ++ show s

xml2State :: InXML a => Exercise (Context a) -> XML -> TAS.State a
xml2State ex xml@(Tag "state" _ _) = fromMaybe (error "invalid state in request") $ do
   sp   <- extractText "prefix"  xml
   sc   <- Just $ maybe "" id $ extractText "context" xml
   x    <- findChild (isTag "OMOBJ") xml
   expr <- fromXML x
   let state  = TAS.State ex (Just (makePrefix (read sp) $ strategy ex)) term
       contxt = fromMaybe (error $ "invalid context" ++ show sc) $ parseContext sc
       term   = fmap (const expr) contxt
   return state
xml2State _ _ = error "expected a state tag"

state2xml :: InXML a => TAS.State a -> XML
state2xml state = Tag "state" [] 
   [ Tag "prefix"  [] [Text $ maybe "[]" show (TAS.prefix state)]
   , Tag "context" [] [Text $ showContext (TAS.term state)] 
   , toXML (fromContext (TAS.term state))
   ]
   
getState :: Monad m => XML -> m X
getState xml =
   case findChild (isTag "state") xml of
      Just a -> maybe (fail "invalid xml state") return (fromXML a)
      _ -> fail "expected tag state"

resultOk :: XML -> XML
resultOk = Tag "reply" [("result", "ok"), ("version", "0.1")] . return

data X = forall a . InXML a => X (TAS.State a)

instance InXML Expr where
   toXML   = omobj2xml . fromExpr
   fromXML = either fail (return . toExpr) . xml2omobj

instance InXML X where
   toXML (X s) = state2xml s
   fromXML xml = return $ X (xml2State derivativeExercise xml)

instance InXML Bool where
   toXML = Text . show
   fromXML (Text s) = case reads s of
                         [(b, rest)] | all isSpace rest -> return b
                         _ -> fail "expecting a boolean"
   fromXML _ = fail "expecting text"

instance InXML Int where
   toXML = Text . show
   fromXML (Text s) = case reads s of
                         [(n, rest)] | all isSpace rest -> return n
                         _ -> fail "expecting an int"
   fromXML _ = fail "expecting text"