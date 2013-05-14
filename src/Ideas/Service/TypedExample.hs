{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Ideas.Service.TypedExample (typedExample) where

import Ideas.Common.Library
import System.IO.Error
import Control.Monad.Error
import Data.Char
import Data.Maybe
import Ideas.Service.DomainReasoner
import Ideas.Service.Evaluator
import Ideas.Service.ModeXML
import Ideas.Service.Types
import Ideas.Text.XML

typedExample :: DomainReasoner -> Exercise a -> Service -> [TypedValue (Type a)] -> IO (XML, XML, Bool)
typedExample dr ex service args = do
   let noXML = makeXML "xml" (return ()) -- quick fix
   -- Construct a request in xml
   request <-
      case makeArgType args of
         Nothing -> return $
            stdReply (showId service) enc ex (return ())
         Just tv -> do
            xml <- runEval dr (encoder evaluator tv) noXML
            return $
               stdReply (showId service) enc ex xml
   -- Construct a reply in xml
   reply <- do
      let tv = foldl dynamicApply (serviceFunction service) args
      xml <- runEval dr (encoder evaluator tv) noXML
      return (resultOk xml)
    `catchError`
      (return . resultError . ioeGetErrorString)
   -- Check request/reply pair
   xmlTest <- do
      (_, txt, _) <- processXML dr (show request)
      let p   = filter (not . isSpace)
          out = showXML (addVersion (version dr) reply)
      return (p txt == p out)
     `catchError`
      const (return False)
   return (request, reply, xmlTest)
 where
   (evaluator, enc)
      | isJust (hasTermView ex) = (openMathConverter False ex, "openmath")
      | otherwise               = (stringFormatConverter ex, "string")

stdReply :: String -> String -> Exercise a -> XMLBuilder -> XML
stdReply s enc ex body = makeXML "request" $ do
   "service"    .=. s
   "exerciseid" .=. showId ex
   "source"     .=. "test"
   "encoding"   .=. enc
   body

makeArgType :: [TypedValue (Type a)] -> Maybe (TypedValue (Type a))
makeArgType []   = fail "makeArgType: empty list"
makeArgType [_ ::: Const Exercise] = fail "makeArgType: empty list"
makeArgType [tv] = return tv
makeArgType ((a1 ::: t1) : rest) = do
   a2 ::: t2 <- makeArgType rest
   return $ (a1, a2) ::: Pair t1 t2

dynamicApply :: Equal f => TypedValue (TypeRep f) -> TypedValue (TypeRep f) -> TypedValue (TypeRep f)
dynamicApply fun arg =
   case (fun, arg) of
      (f ::: t1 :-> t2, a ::: t3) ->
         case equal t3 t1 of
            Just eq -> f (eq a) ::: t2
            Nothing -> error $ "mismatch (argument type)"
      _ -> error "mismatch (not a function)"