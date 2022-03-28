{-# LANGUAGE GADTs #-}
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
              g <- equalM tDifficulty tp
              a <- decodeAttribute "difficulty"
              maybe (fail "unknown difficulty level") (return . g) (readDifficulty a)
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
            Term        -> get >>= (fromXML' >=> maybe (fail "invalid OpenMath") return . fromOMOBJ)
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
            _ -> fail $ "No support for argument type in XML: " ++ show tp
      _ -> fail $ "No support for argument type in XML: " ++ show tp

-- <ruleid>
decodeRule :: XMLDecoder a (Rule (Context a))
decodeRule = decodeChild "ruleid" $ do
   ex  <- getExercise
   xml <- get
   maybe (fail "invalid rule") return . getRule ex . newId . getData $ xml

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
   prefixText <- gets (maybe "" getData . findChild "prefix")
   if all isSpace prefixText
      then return [emptyPath]
      else if prefixText ~= "no prefix"
      then return []
      else maybe (fail "invalid paths") return (readPaths prefixText)
 where
   a ~= b = g a == g b
   g = map toLower . filter (not . isSpace)

decodeContext :: XMLDecoder a (Context a)
decodeContext = do
   ex   <- getExercise
   expr <- decodeExpression
   env  <- decodeEnvironment
   let ctx    = setEnvironment env (inContext ex expr)
       locRef = makeRef "location"
   case locRef ? env of
      Just s  -> maybe (fail "invalid location") return $ do
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
      get >>= either fail return . parser ex . getData

decodeOMOBJ :: XMLDecoder a a
decodeOMOBJ = decodeChild "OMOBJ" $ get >>= \xml -> do
   ex    <- getExercise
   omobj <- fromXML' xml
   case fromOpenMath ex omobj of
      Just a  -> return a
      Nothing -> fail "Invalid OpenMath object for this exercise"

decodeMathML :: XMLDecoder a MathML
decodeMathML = decodeFirstChild "math" $ get >>= fromXML'

decodeEnvironment :: XMLDecoder a Environment
decodeEnvironment =
   decodeChild "context" (get >>= foldM add mempty . children)
   <|> return mempty
 where
   add env item = do
      unless (show (name item) == "item") $
         fail $ "expecting item tag, found " ++ show (name item)
      n   <- findAttribute "name"  item
      req <- getRequest
      case findChild "OMOBJ" item of
         -- OpenMath object found inside item tag
         Just this | useOpenMath req ->
            case xml2omobj this of
               Left err -> fail err
               Right omobj ->
                  case fromOMOBJ omobj of
                     Just term -> return $ insertRef (makeRef n) (term :: Term) env
                     Nothing -> fail "invalid openmath"
         -- Simple value in attribute
         _ -> do
            value <- findAttribute "value" item
            return $ insertRef (makeRef n) value env

-- <configuration>
decodeConfiguration :: XMLDecoder a StrategyCfg
decodeConfiguration = decodeChild "configuration" $
   get >>= \xml ->
      mconcat <$> mapM decodeAction (children xml)
 where
   decodeAction item = do
      guard (null (children item))
      action <- maybe (fail "invalid action") return $ readM (show (name item))
      cfgloc <- findAttribute "name" item
      return (action `byName` newId cfgloc)

decodeArgEnvironment :: XMLDecoder a Environment
decodeArgEnvironment = get >>=
   fmap makeEnvironment . mapM (decodeBinding //) . findChildren "argument"

decodeBinding :: XMLDecoder a Binding
decodeBinding = get >>= \xml -> do
   a   <- findAttribute "description" xml
   req <- getRequest
   case findChild "OMOBJ" xml of
      -- OpenMath object found inside tag
      Just this | useOpenMath req ->
         case xml2omobj this of
            Left err   -> fail err
            Right omobj -> 
               case fromOMOBJ omobj of
                  Just term -> return (termBinding a term)
                  Nothing -> fail "invalid openmath"
      -- Simple value
      _ -> return (makeBinding (makeRef a) (getData xml))
 where
   termBinding :: String -> Term -> Binding
   termBinding = makeBinding . makeRef

fromXML' :: (Monad m, InXML a) => XML -> m a
fromXML' = maybe (fail "fromXML'") return . fromXML