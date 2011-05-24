{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.TypedExample (typedExample) where

import Data.Char
import Service.DomainReasoner
import Service.ModeXML
import Service.ExercisePackage
import Service.Evaluator
import Service.Types
import Common.Library
import Text.XML
   
typedExample :: ExercisePackage a -> Service -> [TypedValue a] -> DomainReasoner (XML, XML, Bool)
typedExample pkg service args = do
   -- Construct a request in xml
   request <- 
      case makeArgType args of
         Nothing -> return $  
            stdReply (showId service) enc pkg (return ())
         Just (reqTuple ::: reqTp) -> do
            xml <- encodeType (encoder evaluator) reqTp reqTuple
            return $ 
               stdReply (showId service) enc pkg xml
   -- Construct a reply in xml
   reply <-
      case foldl dynamicApply (serviceFunction service) args of
         reply ::: replyTp -> do
            xml <- encodeType (encoder evaluator) replyTp reply
            return (resultOk xml) 
    `catchError` 
      (return . resultError)
   -- Check request/reply pair
   vers <- getVersion
   xmlTest <- do
      (_, txt, _) <- processXML (show request)
      let p   = filter (not . isSpace)
          out = showXML (if null vers then reply else addVersion vers reply)
      return (p txt == p out)
     `catchError` 
      const (return False)
   return (request, reply, xmlTest)
 where
   (evaluator, enc)
      | withOpenMath pkg = (openMathConverterTp pkg, "openmath")
      | otherwise        = (stringFormatConverterTp pkg, "string")

stdReply :: String -> String -> Exercise a -> XMLBuilder -> XML
stdReply s enc ex body = makeXML "request" $ do 
   "service"    .=. s
   "exerciseid" .=. showId ex
   "source"     .=. "test"
   "encoding"   .=. enc
   body

makeArgType :: [TypedValue a] -> Maybe (TypedValue a)
makeArgType []   = fail "makeArgType: empty list"
makeArgType [_ ::: ExercisePkg] = fail "makeArgType: empty list"
makeArgType [tv] = return tv
makeArgType ((a1 ::: t1) : rest) = do
   a2 ::: t2 <- makeArgType rest
   return $ (a1, a2) ::: Pair t1 t2

dynamicApply :: TypedValue a -> TypedValue a -> TypedValue a
dynamicApply fun arg =
   case (fun, arg) of
      (f ::: t1 :-> t2, a ::: t3) -> 
         case equal t3 t1 of 
            Just eq -> f (eq a) ::: t2
            Nothing -> error $ "mismatch (argument type): " ++ show t3 ++ " does not match " ++ show t1
      _ -> error "mismatch (not a function)"