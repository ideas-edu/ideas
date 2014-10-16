{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
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
--  $Id$

module Ideas.Encoding.DecoderXML
   ( XMLDecoder, XMLDecoderState(..), xmlDecoder
   ) where

import Control.Monad
import Data.Char
import Ideas.Common.Library hiding (exerciseId, (:=))
import Ideas.Common.Traversal.Navigator
import Ideas.Encoding.Evaluator
import Ideas.Encoding.OpenMathSupport
import Ideas.Service.FeedbackScript.Syntax (Script)
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import System.Random (StdGen)

type XMLDecoder a = DecoderState (XMLDecoderState a) XML

data XMLDecoderState a = XMLDecoderState
   { getExercise :: Exercise a
   , getScript   :: Script
   , getStdGen   :: StdGen
   , isOpenMath  :: Bool
   , decodeTerm  :: XML -> Either String a
   }

xmlDecoder :: Type a t -> XMLDecoder a t
xmlDecoder tp =
   case tp of
      Tag s t
         | s == "answer" -> do
              decodeChild "answer" (xmlDecoder t)
         | s == "Difficulty" -> do
              g <- equalM typed tp
              a <- decoderFor (findAttribute "difficulty")
              maybe (fail "unknown difficulty level") (return . g) (readDifficulty a)
         | otherwise -> do
              decodeChild s (xmlDecoder t)
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
            StdGen      -> withStateD getStdGen
            Script      -> withStateD getScript
            Exercise    -> withStateD getExercise
            Id          -> -- improve! 
                           decodeChild "location" $ 
                              simpleDecoder (newId . getData)
            _ -> fail $ "No support for argument type in XML: " ++ show tp
      _ -> fail $ "No support for argument type in XML: " ++ show tp

-- <ruleid>
decodeRule :: XMLDecoder a (Rule (Context a))
decodeRule = decodeChild "ruleid" $ do
   ex <- withStateD getExercise
   decoderFor (getRule ex . newId . getData)

-- <location>
decodeLocation :: XMLDecoder a Location
decodeLocation = decodeChild "location" $ do
   simpleDecoder (toLocation . read . getData)

-- <state> 
decodeState :: XMLDecoder a (State a)
decodeState = decodeChild "state"  $ do
   ps  <- decodePaths
   ctx <- decodeContext
   withStateD $ \st -> 
      let ex  = getExercise st
          prf = replayPaths ps (strategy ex) ctx
      in makeState ex prf ctx

-- <prefix>
decodePaths :: XMLDecoder a [Path]
decodePaths = do
   prefixText <- simpleDecoder (maybe "" getData . findChild "prefix")
   if all isSpace prefixText
      then return [emptyPath]
      else if prefixText ~= "no prefix"
      then return []
      else readPaths prefixText
 where
   a ~= b = g a == g b
   g = map toLower . filter (not . isSpace)

decodeContext :: XMLDecoder a (Context a)
decodeContext = do
   ex   <- withStateD getExercise
   f    <- withStateD decodeTerm
   expr <- decoderFor (either fail return . f)
   env  <- decodeEnvironment
   let ctx    = setEnvironment env (inContext ex expr)
       locRef = makeRef "location" 
   case locRef ? env of 
      Just s  -> maybe (fail "invalid location") return $ do 
         loc <- liftM toLocation (readM s)
         navigateTo loc (deleteRef locRef ctx)
      Nothing -> 
         return ctx

decodeEnvironment :: XMLDecoder a Environment
decodeEnvironment = decoderFor $ \xml ->
   case findChild "context" xml of
      Just this -> foldM add mempty (children this)
      Nothing   -> return mempty
 where
   add env item = do
      unless (name item == "item") $
         fail $ "expecting item tag, found " ++ name item
      n    <- findAttribute "name"  item
      isOM <- withStateD isOpenMath
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

-- <configuration>
decodeConfiguration :: XMLDecoder a StrategyCfg
decodeConfiguration = decodeChild "configuration" $
   decoderFor $ \xml -> 
      liftM mconcat $
         mapM decodeAction (children xml)
 where
   decodeAction item = do
      guard (null (children item))
      action <- readM (name item)
      cfgloc <- findAttribute "name" item
      return (action `byName` newId cfgloc)

decodeArgEnvironment :: XMLDecoder a Environment
decodeArgEnvironment = decoderFor $
   liftM makeEnvironment . mapM (decodeBinding ///) . findChildren "argument"

decodeBinding :: XMLDecoder a Binding
decodeBinding = decoderFor $ \xml -> do
   a <- findAttribute "description" xml
   isOM <- withStateD isOpenMath
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
   
decodeChild :: String -> XMLDecoder a b -> XMLDecoder a b
decodeChild s m = decoderFor $ \xml -> 
   case break (either (const False) ((==s) . name)) (content xml) of
      (xs, Right y:ys) -> do
         a <- m /// y
         setInput xml { content = xs ++ ys }
         return a
      _ -> fail ("Could not find child " ++ s)