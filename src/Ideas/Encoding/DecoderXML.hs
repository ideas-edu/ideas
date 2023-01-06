{-# LANGUAGE OverloadedStrings, GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Services using XML notation
--
-----------------------------------------------------------------------------

module Ideas.Encoding.DecoderXML
   ( XMLDecoder, xmlTypeDecoder
   ) where

import Control.Applicative hiding (Const)
import Control.Monad.State hiding (State)
import Data.Char
import Ideas.Common.Library
import Ideas.Common.Traversal.Navigator
import Ideas.Encoding.Encoder
import Ideas.Encoding.OpenMathSupport
import Ideas.Encoding.Request hiding (XML)
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Text.MathML
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import Ideas.Utils.Decoding

type XMLDecoder a t = DecoderX a String XML t

xmlTypeDecoder :: TypedDecoder a XML
xmlTypeDecoder tp =
   case tp of
      Tag s (Const String) ->
         decodeChild s decodeData <|> decodeAttribute s
      Tag s t
         | s == "answer" ->
              decodeChild "answer" (xmlTypeDecoder t)
         | s == "Difficulty" -> do
              g <- either errorStr return (equalM tDifficulty tp)
              a <- decodeAttribute "difficulty"
              maybe (errorStr "unknown difficulty level") (return . g) (readDifficulty a)
         | otherwise ->
              decodeChild s (xmlTypeDecoder t)
      Iso p t  -> from p <$> xmlTypeDecoder t
      List t -> do
         x  <- xmlTypeDecoder t
         xs <- xmlTypeDecoder (List t)
         return (x:xs)
       <|>
         return []
      Pair t1 t2 -> do
         x <- xmlTypeDecoder t1
         y <- xmlTypeDecoder t2
         return (x, y)
      t1 :|: t2 ->
         Left  <$> xmlTypeDecoder t1 <|> Right <$> xmlTypeDecoder t2
      Unit -> return ()
      Const ctp ->
         case ctp of
            State       -> decodeState
            Context     -> decodeContext
            Rule        -> decodeRule
            Environment -> decodeArgEnvironment
            Term        -> get >>= (fromXML' >=> maybe (errorStr "invalid OpenMath") return . fromOMOBJ)
            Location    -> decodeLocation
            StratCfg    -> decodeConfiguration
            QCGen       -> getQCGen
            Script      -> getScript
            Exercise    -> getExercise
            Id          -> -- improve!
                           decodeChild "location" $
                              gets (newId . getData)
            MathML      -> decodeMathML
            String      -> decodeData
            XML         -> get
            _ -> errorStr $ "No support for argument type in XML: " ++ show tp
      _ -> errorStr $ "No support for argument type in XML: " ++ show tp

-- <ruleid>
decodeRule :: XMLDecoder a (Rule (Context a))
decodeRule = decodeChild "ruleid" $ do
   ex  <- getExercise
   xml <- get
   maybe (errorStr "invalid rule") return . getRule ex . newId . getData $ xml

-- <location>
decodeLocation :: XMLDecoder a Location
decodeLocation = decodeChild "location" $
   gets (toLocation . read . getData)

-- <state>
decodeState :: XMLDecoder a (State a)
decodeState = decodeChild "state"  $ do
   ex  <- getExercise
   ps  <- decodePaths
   ctx <- decodeContext
   let prf = replayPaths ps (strategy ex) ctx
   return (makeState ex prf ctx)

-- <prefix>
decodePaths :: XMLDecoder a [Path]
decodePaths = do
   prefixText <- gets (either (const "") getData . findChild "prefix")
   if all isSpace prefixText
      then return [emptyPath]
      else if prefixText ~= "no prefix"
      then return []
      else maybe (errorStr "invalid paths") return (readPaths prefixText)
 where
   a ~= b = g a == g b
   g = map toLower . filter (not . isSpace)

decodeContext :: XMLDecoder a (Context a)
decodeContext = do
   ex   <- getExercise
   expr <- decodeExpression
   env  <- decodeEnvironment
   let ctx    = setEnvironment env (inContext ex expr)
       locRef = makeRef ("location" :: String)
   case locRef ? env of
      Just s  -> maybe (errorStr "invalid location") return $ do
         loc <- toLocation <$> readM s
         navigateTo loc (deleteRef locRef ctx)
      Nothing ->
         return ctx

decodeExpression :: XMLDecoder a a
decodeExpression = withOpenMath f
 where
   f True  = decodeOMOBJ
   f False = decodeChild "expr" $ do
      ex <- getExercise
      get >>= either errorStr return . parser ex . getData

decodeOMOBJ :: XMLDecoder a a
decodeOMOBJ = decodeChild "OMOBJ" $ get >>= \xml -> do
   ex    <- getExercise
   omobj <- fromXML' xml
   case fromOpenMath ex omobj of
      Just a  -> return a
      Nothing -> errorStr "Invalid OpenMath object for this exercise"

decodeMathML :: XMLDecoder a MathML
decodeMathML = decodeFirstChild "math" $ get >>= fromXML'

decodeEnvironment :: XMLDecoder a Environment
decodeEnvironment =
   decodeChild "context" (get >>= foldM add mempty . children)
   <|> return mempty
 where
   add env item = do
      unless (getName item == "item") $
         errorStr $ "expecting item tag, found " ++ show (getName item)
      n   <- findAttribute' "name"  item
      req <- getRequest
      case findChild "OMOBJ" item of
         -- OpenMath object found inside item tag
         Right this | useOpenMath req ->
            case xml2omobj this of
               Left err -> errorStr err
               Right omobj ->
                  case fromOMOBJ omobj of
                     Just term -> return $ insertRef (makeRef n) (term :: Term) env
                     Nothing -> errorStr "invalid openmath"
         -- Simple value in attribute
         _ -> do
            value <- findAttribute' "value" item
            return $ insertRef (makeRef n) value env

-- <configuration>
decodeConfiguration :: XMLDecoder a StrategyCfg
decodeConfiguration = decodeChild "configuration" $
   get >>= \xml ->
      mconcat <$> mapM decodeAction (children xml)
 where
   decodeAction item = do
      guard (null (children item))
      action <- maybe (errorStr "invalid action") return $ readM (show (getName item))
      cfgloc <- findAttribute' "name" item
      return (action `byName` newId cfgloc)

decodeArgEnvironment :: XMLDecoder a Environment
decodeArgEnvironment = get >>=
   fmap makeEnvironment . mapM (decodeBinding //) . findChildren "argument"

decodeBinding :: XMLDecoder a Binding
decodeBinding = get >>= \xml -> do
   a   <- findAttribute' "description" xml
   req <- getRequest
   case findChild "OMOBJ" xml of
      -- OpenMath object found inside tag
      Right this | useOpenMath req ->
         case xml2omobj this of
            Left err   -> errorStr err
            Right omobj -> 
               case fromOMOBJ omobj of
                  Just term -> return (termBinding a term)
                  Nothing -> errorStr "invalid openmath"
      -- Simple value
      _ -> return (makeBinding (makeRef a) (getData xml))
 where
   termBinding :: String -> Term -> Binding
   termBinding = makeBinding . makeRef

fromXML' :: InXML a => XML -> XMLDecoder s a
fromXML' = maybe (errorStr "fromXML'") return . fromXML

findAttribute' :: String -> XML -> XMLDecoder a String
findAttribute' s = either errorStr return . findAttribute s