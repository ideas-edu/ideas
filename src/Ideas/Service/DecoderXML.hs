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
-- Services using XML notation
--
-----------------------------------------------------------------------------
module Ideas.Service.DecoderXML 
   ( XMLDecoder, XMLDecoderState(..), xmlDecoder
   ) where

import Ideas.Common.Library hiding (exerciseId, (:=))
import Ideas.Common.Utils (readM)
import Control.Monad
import Data.Char
import Data.List
import Ideas.Service.Evaluator
import Ideas.Service.OpenMathSupport
import Ideas.Service.State
import Ideas.Service.FeedbackScript.Syntax (Script)
import Ideas.Service.Types
import System.Random (StdGen)
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML

type XMLDecoder a = EncoderState (XMLDecoderState a) XML

data XMLDecoderState a = XMLDecoderState
   { getExercise       :: Exercise a
   , getScript         :: Script
   , getStdGen         :: StdGen
   , isOpenMath        :: Bool
   , decodeTerm        :: XML -> Either String a
   }

xmlDecoder :: Type a t -> XMLDecoder a t
xmlDecoder tp =
   case tp of
      Tag s t
         | s == "answer" -> do
              c <- encoderFor (findChild "answer")
              xmlDecoder t // c 
         | s == "Difficulty" -> do
              g <- equalM typed tp
              a <- encoderFor (findAttribute "difficulty")
              maybe (fail "unknown difficulty level") (return . g) (readDifficulty a)
         | otherwise -> do
              cx <- encoderFor (findChild s)
              xmlDecoder t // cx
      Iso p t  -> liftM (from p) (xmlDecoder t) 
      Pair t1 t2 -> do
         x <- xmlDecoder t1
         y <- xmlDecoder t2
         return (x, y) 
      t1 :|: t2 ->
         liftM Left  (xmlDecoder t1) `mplus`
         liftM Right (xmlDecoder t2)
      Unit -> return ()
      Const ctp -> 
         case ctp of
            State       -> decodeState
            Context     -> decodeContext
            Rule        -> decodeRule
            Environment -> decodeArgEnvironment
            Location    -> decodeLocation
            StratCfg    -> decodeConfiguration
            StdGen      -> withState getStdGen
            Script      -> withState getScript
            Exercise    -> withState getExercise
            Id          -> do -- improve!
                              a <- encoderFor (findChild "location")
                              return (newId (getData a))
            _ -> fail $ "No support for argument type in XML: " ++ show tp
      _ -> fail $ "No support for argument type in XML: " ++ show tp

decodeRule :: XMLDecoder a (Rule (Context a))
decodeRule = do
   ex <- withState getExercise
   xml0 <- encoderFor (findChild "ruleid")
   getRule ex (newId (getData xml0))

decodeLocation :: XMLDecoder a Location
decodeLocation = do
   xml <- encoderFor (findChild "location")
   return (toLocation (read (getData xml)))

decodeState :: XMLDecoder a (State a)
decodeState = do
   ex   <- withState getExercise
   xml  <- encoderFor (findChild "state")
   mpr  <- decodePrefix  // xml
   term <- decodeContext // xml
   return (makeState ex mpr term)

decodePrefix :: XMLDecoder a [Prefix (Context a)]
decodePrefix = do
   str <- liftM strategy (withState getExercise)
   prefixText <- simpleEncoder (maybe "" getData . findChild "prefix")
   if all isSpace prefixText
      then return [emptyPrefix str]
      else if prefixText ~= "no prefix"
      then return []
      else do
         a  <- readM prefixText
         pr <- makePrefix a str
         return [pr]
 where
   a ~= b = g a == g b
   g = map toLower . filter (not . isSpace) 

decodeContext :: XMLDecoder a (Context a)
decodeContext = do
   ex   <- withState getExercise
   f    <- withState decodeTerm
   expr <- encoderFor (either fail return . f)
   env  <- decodeEnvironment
   return (makeContext ex env expr)

decodeEnvironment :: XMLDecoder a Environment
decodeEnvironment = encoderFor $ \xml ->
   case findChild "context" xml of
      Just this -> foldM add mempty (children this)
      Nothing   -> return mempty
 where
   add env item = do
      unless (name item == "item") $
         fail $ "expecting item tag, found " ++ name item
      n    <- findAttribute "name"  item
      isOM <- withState isOpenMath
      case findChild "OMOBJ" item of
         -- OpenMath object found inside item tag
         Just this | isOM ->
            case xml2omobj this >>= fromOMOBJ of
               Left err -> fail err
               Right term ->
                  return $ insertRef (makeRef n) (term :: Term) env
         -- Simple value in attribute
         _ -> do
            value <- findAttribute "value" item
            return $ insertRef (makeRef n) value env

decodeConfiguration :: XMLDecoder a StrategyConfiguration
decodeConfiguration = do
   xml <- encoderFor (findChild "configuration")
   liftM makeStrategyConfiguration $ 
      mapM decodeAction (children xml)
 where
   decodeAction item = do
      guard (null (children item))
      action <-
         case find (\a -> map toLower (show a) == name item) configActions of
            Just a  -> return a
            Nothing -> fail $ "unknown action " ++ show (name item)
      cfgloc <- findAttribute "name" item
      return (byName (newId cfgloc), action)

decodeArgEnvironment :: XMLDecoder a Environment
decodeArgEnvironment = encoderFor $ \xml ->
   liftM makeEnvironment $ sequence
      [ decodeBinding // x
      | x <- children xml
      , name x == "argument"
      ]
 
decodeBinding :: XMLDecoder a Binding
decodeBinding = encoderFor $ \xml -> do
   a <- findAttribute "description" xml
   isOM <- withState isOpenMath
   case findChild "OMOBJ" xml of
      -- OpenMath object found inside tag
      Just this | isOM -> 
         case xml2omobj this >>= fromOMOBJ of
            Left err   -> fail err
            Right term -> return (termBinding a term)
      -- Simple value
      _ -> return (makeBinding (makeRef a) (getData xml))
 where
   termBinding :: String -> Term -> Binding
   termBinding = makeBinding . makeRef