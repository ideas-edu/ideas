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
-- Analysis of a feedbackscript
--
-----------------------------------------------------------------------------
module Service.FeedbackScript.Analysis (withScripts) where

import Common.Exercise
import Common.Transformation
import Common.Id
import Common.Uniplate
import Common.Utils (Some(..))
import Control.Monad
import Data.Either
import Data.List
import Service.DomainReasoner
import Service.ExercisePackage
import Service.FeedbackScript.Syntax
import Service.FeedbackScript.Parser
import Service.FeedbackScript.Run 

withScripts :: Maybe FilePath -> [String] -> [FilePath] -> DomainReasoner ()
withScripts path xs ys = do 
   -- generate scripts
   forM_ xs $ \s -> do
      Some pkg <- findPackage (newId s)
      liftIO $ print (generateScript pkg)
   -- analyze scripts
   forM_ ys $ \file -> do
      liftIO $ putStrLn $ "Parsing " ++ show file
      script <- liftIO $ parseScript path file
      let sups = [ a | Supports as <- scriptDecls script, a <- as ]
      exs <- forM sups $ \a -> do
                liftM Right (findPackage a)
              `catchError` \_ -> return $ Left $ UnknownExercise a
           
      let ms = lefts exs ++ analyzeScript (rights exs) script
      liftIO $ do 
         putStrLn $ unlines $ map show ms
         putStrLn $ "(errors: " ++ show (length ms) ++ ")"

generateScript :: Exercise a -> Script
generateScript ex = makeScript $
   Supports [getId ex] :
   [ feedbackDecl s mempty | s <- feedbackIds ] ++
   [ textForIdDecl r (makeText (description r)) | r <- nrs ] ++
   [ textForIdDecl r (makeText (description r)) | r <- brs ]
 where
   (brs, nrs) = partition isBuggyRule (ruleset ex)
   
data Message = UnknownExercise   Id
             | UnknownFeedback   Id
             | FeedbackUndefined Id
             | NoTextForRule Id Id
             | UnknownAttribute Id
             | UnknownCondAttr  Id

instance Show Message where
   show message = 
      case message of
         UnknownExercise a   -> "Unknown exercise id " ++ show a
         UnknownFeedback a   -> "Unknown feedback category " ++ show a
         FeedbackUndefined a -> "Feedback category " ++ show a ++ " is not defined"
         NoTextForRule a b   -> "No text for rule " ++ show a ++ " of exercise " ++ show b
         UnknownAttribute a  -> "Unknown attribute @" ++ show a ++ " in text"
         UnknownCondAttr a   -> "Unknown attribute @" ++ show a ++ " in condition"
   
analyzeScript :: [Some ExercisePackage] -> Script -> [Message]
analyzeScript exs script =
   map FeedbackUndefined (filter (`notElem` fbids) feedbackIds) ++ 
   map UnknownFeedback   (filter (`notElem`feedbackIds ) fbids) ++ 
   [ NoTextForRule (getId r) (getId ex) 
   | Some ex <- exs, r <- ruleset ex, noTextFor (getId r)
   ] ++
   [ UnknownAttribute a | a <- textRefs
   , a `notElem` feedbackIds ++ attributeIds ++ strids ] ++
   [ UnknownCondAttr a | a <- condRefs, a `notElem` conditionIds ] 
 where
   decls = scriptDecls script
   fbids = [ a | Simple  Feedback as _ <- decls, a <- as ] ++
           [ a | Guarded Feedback as _ <- decls, a <- as ]
   txids = [ a | Simple  TextForId as _ <- decls, a <- as ] ++
           [ a | Guarded TextForId as _ <- decls, a <- as ]
   strids = [ a | Simple  StringDecl as _ <- decls, a <- as ] ++
            [ a | Guarded StringDecl as _ <- decls, a <- as ]
   namespaces = nub $ mempty : [ a | NameSpace as <- scriptDecls script, a <- as ]
   noTextFor a = null [ () | n <- namespaces, b <- txids, (n#b) == a ]
        
   texts = [ t | Simple  _ _ t <- decls ] ++
           [ t | Guarded _ _ xs <- decls, (_, t) <- xs ]
   textRefs = [ a | t <- texts, TextRef a <- universe t ]
   
   conditions  = [ c | Guarded _ _ xs <- decls , (c, _) <- xs ]
   condRefs = [ a | c <- conditions, CondRef a <- universe c ]