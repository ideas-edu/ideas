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
import Domain.Math.Expr
import OpenMath.Object
import Service.XML
import Service.AbstractService (getExercise)
import Service.Revision (version)
import OpenMath.LAServer
import OpenMath.Reply
import OpenMath.Interactive (respondHTML)
import OpenMath.Conversion
import Domain.Math.DerivativeExercise
import Domain.Math.LinearEquations
import qualified Service.TypedAbstractService as TAS
import Data.Maybe
import Data.Char


import Domain.Math.Equation
vb :: Equation Expr
vb = 1 - ((4*x + 2) / 3) :==: 3*x - ((5*x - 1) /4)
 where x = Var "x"

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
   let ec = extractExerciseCode xml 
   serviceXML (map toLower s) ec xml

extractExerciseCode ::XML -> ExerciseCode
extractExerciseCode xml =
   case findAttribute "exerciseid" xml >>= return . break (=='.') of
      Just (as, _:bs) -> makeCode as bs
      Just (as, _)    -> makeCode "" as
      Nothing         -> makeCode "" "" -- being backwards compatible with early MathDox

-- temporary
serviceXML :: String -> ExerciseCode -> XML -> IO XML
serviceXML s ec request
   | s == "derivation" = do
        X state <- getState ec request
        let list   = TAS.derivation state
            f (r, a) = element "elem" $ do 
               "ruleid" .=. show r
               builder $ toXML $ fromContext a
        return $ resultOk $ element "list" (mapM_ f list)
   | s == "allfirsts" = do
        X state <- getState ec request
        let list   = TAS.allfirsts state
            f (r, _, a) = element "elem" $ do
               "ruleid" .=. show r
               builder $ state2xml a
        return $ resultOk $ element "list" (mapM_ f list)
   | s == "onefirst" = do
        X state <- getState ec request
        let this = TAS.onefirst state
            f (r, _, a) = element "elem" $ do
               "ruleid" .=. show r
               builder (state2xml a)
        return $ resultOk $ f this
   | s == "ready" = do
        X state <- getState ec request
        let a = TAS.ready state
        return $ resultOk $ text $ show a
   | s == "stepsremaining" = do
        X state <- getState ec request
        let a = TAS.stepsremaining state
        return $ resultOk $ text $ show a
   | s == "applicable" = do
        X state <- getState ec request
        let loc   = maybe (error "no location") getData (findChild "location" request)         
            list  = TAS.applicable (read loc) state
            f r   = element "elem" ("ruleid" .=. show r)
        return $ resultOk $ element "list" (mapM_ f list)
   | s == "apply" = do
        X state <- getState ec request
        let rid   = maybe (error "no ruleid") getData (findChild "ruleid" request)
            rule  = fromMaybe (error "invalid ruleid") $ safeHead $ filter p (ruleset (TAS.exercise state))
            p     = (==rid) . Transformation.name
            loc   = maybe (error "no location") getData (findChild "location" request)
            this  = TAS.apply rule (read loc) state
        return $ resultOk $ builder $ state2xml this
   | s == "generate" =
        case getExercise "Derivative" of  -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!
           Some _ -> do
              let diff  = fromMaybe (error "no difficulty") $ findAttribute "difficulty" request 
              state <- TAS.generate derivativeExercise (read diff) -- !!!!!!!!!!!!!!!!!!!!!!!!!!!
              return $ resultOk $ builder $ state2xml state
   | s == "mathdox" = do
        req <- fromXML request
        return $ replyToXML $ laServer req

serviceXML s _ _ = fail $ "Invalid request: unknown service " ++ show s


xml2State :: InXML a => Exercise a -> XML -> TAS.State a
xml2State ex xml = fromMaybe (error "invalid state in request") $ do
   unless (name xml == "state") (fail "expected a state tag")
   sp   <- liftM getData (findChild "prefix" xml)
   sc   <- Just $ maybe "" getData $ findChild "context" xml
   x    <- findChild "OMOBJ" xml
   expr <- fromXML x
   let state  = TAS.State ex (Just (makePrefix (read sp) $ strategy ex)) term
       contxt = fromMaybe (error $ "invalid context" ++ show sc) $ parseContext sc
       term   = fmap (const expr) contxt
   return state

state2xml :: InXML a => TAS.State a -> XML
state2xml state = makeXML "state" $ do
   element "prefix"  (text $ maybe "[]" show (TAS.prefix state))
   element "context" (text $ showContext (TAS.context state))
   builder (toXML (TAS.term state))
   
getState :: Monad m => ExerciseCode -> XML -> m X
getState ec xml =
   case findChild "state" xml of
      Just a 
         | ec == exerciseCode linearEquationExercise -> 
              return $ X (xml2State linearEquationExercise a)
         | ec == exerciseCode derivativeExercise -> 
              return $ X (xml2State derivativeExercise a)
         | otherwise ->
              fail $ "unknown exercise id: " ++ show ec
      _ ->    fail "expected tag state"

resultOk :: XMLBuilder -> XML
resultOk body = makeXML "reply" $ do 
   "result"  .=. "ok"
   "version" .=. version
   body

data X = forall a . InXML a => X (TAS.State a)

instance InXML Expr where
   toXML   = omobj2xml . toOMOBJ
   fromXML = either fail (maybe (fail "Conversion from OMOBJ to Expr") return . fromOMOBJ) . xml2omobj

instance IsOMOBJ a => InXML (Equation a) where
   toXML eq = omobj2xml (toOMOBJ eq)
   fromXML = either fail (maybe (fail "Conversion from OMOBJ to Equation") return . fromOMOBJ) . xml2omobj