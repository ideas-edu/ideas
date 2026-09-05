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
-- Analysis of a feedbackscript
--
-----------------------------------------------------------------------------

module Ideas.Service.FeedbackScript.Analysis
   ( -- Analysis functions
     makeScriptFor, parseAndAnalyzeScript, analyzeScript
     -- Message type
   , Message(..)
   ) where

import Data.Either
import Data.List
import Ideas.Common.Library
import Ideas.Service.DomainReasoner
import Ideas.Service.FeedbackScript.Parser
import Ideas.Service.FeedbackScript.Run
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Utils.Uniplate

makeScriptFor :: IsId a => DomainReasoner -> a -> IO ()
makeScriptFor dr exId = do
   Some ex <- either fail return $ findExercise dr (newId exId)
   let (brs, nrs) = partition isBuggy (ruleset ex)
   print $ makeScript $
      Supports [getId ex] :
      [ feedbackDecl s mempty | s <- feedbackIds ] ++
      [ textForIdDecl r (makeText (description r)) | r <- nrs ] ++
      [ textForIdDecl r (makeText (description r)) | r <- brs ]

parseAndAnalyzeScript :: DomainReasoner -> FilePath -> IO ()
parseAndAnalyzeScript dr file = do
   putStrLn $ "Parsing " ++ show file
   script <- parseScript file
   let exs = [ either (const unknown) Right (findExercise dr a)
              | Supports as <- scriptDecls script
              , a <- as
              , let unknown = Left (UnknownExercise a)
              ]
   let ms = lefts exs ++ analyzeScript (rights exs) script
   putStrLn $ unlines $ map show ms
   putStrLn $ "(errors: " ++ show (length ms) ++ ")"

analyzeScript :: [Some Exercise] -> Script -> [Message]
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
   noTextFor a = null [ () | n <- namespaces, b <- txids, n#b == a ]

   texts = [ t | Simple  _ _ t <- decls ] ++
           [ t | Guarded _ _ xs <- decls, (_, t) <- xs ]
   textRefs = [ a | t <- texts, TextRef a <- universe t ]

   conditions  = [ c | Guarded _ _ xs <- decls , (c, _) <- xs ]
   condRefs = [ a | c <- conditions, CondRef a <- universe c ]

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