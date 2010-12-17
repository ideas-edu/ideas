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
module Documentation.RulePage (makeRulePages) where

import Common.Context
import Common.Exercise
import Common.Transformation
import Common.Utils (commaList, Some(..))
import Control.Monad
import Data.List
import Documentation.DefaultPage
import Documentation.RulePresenter
import Service.DomainReasoner
import Service.ExercisePackage
import Service.RulesInfo (rewriteRuleToFMP, collectExamples, ExampleMap)
import Text.HTML
import Text.OpenMath.FMP
import Text.OpenMath.Object
import qualified Data.Map as M
import qualified Text.XML as XML

data ExItem a = EI (ExercisePackage a) (ExampleMap a)

makeRulePages :: String -> DomainReasoner ()
makeRulePages dir = do
   pkgs <- getPackages 
   let exMap = M.fromList 
          [ (getId pkg, Some (EI pkg (collectExamples (exercise pkg))))
          | Some pkg <- pkgs
          ]
       ruleMap = M.fromListWith (++)
          [ (getId r, [Some pkg]) 
          | Some pkg <- pkgs
          , r <- ruleset (exercise pkg) 
          ]
   forM_ (M.toList ruleMap) $ \(ruleId, list@(Some pkg:_)) -> do
      let noExamples = Some (EI pkg M.empty) 
          level      = length (qualifiers ruleId) + 1
          usedIn     = sortBy compareId [ getId pkg1 | Some pkg1 <- list ]
      case M.findWithDefault noExamples (getId pkg) exMap of
         Some (EI pkg1 e) -> do
            let ex = exercise pkg1
            forM_ (getRule ex ruleId) $ \r ->
               generatePageAt level dir (ruleFile ruleId) $
                  rulePage ex e usedIn r

rulePage :: Exercise a -> ExampleMap a -> [Id] ->  Rule (Context a) -> HTMLBuilder
rulePage ex exMap usedIn r = do
   idboxHTML "rule" (getId r)
   let idList = text . commaList . map showId
   para $ table 
      [ [bold $ text "Buggy", text $ showBool (isBuggyRule r)]
      , [bold $ text "Rewrite rule", text $ showBool (isRewriteRule r)]
      , [bold $ text "Groups", idList $ ruleGroups r]
      , [bold $ text "Siblings", idList $ ruleSiblings r] 
      ]
   when (isRewriteRule r) $ para $
      ruleToHTML (Some ex) r

   h3 "Used in exercises"
   let f a = link (up ups ++ exercisePageFile a) (tt $ text $ show a)
       ups = length (qualifiers r) + 1
   ul $ map f usedIn

   -- Examples
   let ys  = M.findWithDefault [] (getId r) exMap
   unless (null ys) $ do
      h3 "Examples"
      forM_ (take 3 ys) $ \(a, b) -> para $ divClass "step" $ pre $ do 
         forTerm ex (inContext ex a)
         forStep ups (getId r, emptyEnv)
         forTerm ex (inContext ex b)
         
   -- FMPS
   let xs = getRewriteRules r
   unless (null xs) $ do
      h3 "Formal Mathematical Properties"
      forM_ xs $ \(Some rr, b) -> para $ do
         let fmp = rewriteRuleToFMP b rr
         highlightXML False $ XML.makeXML "FMP" $ 
            XML.builder (omobj2xml (toObject fmp))

idboxHTML :: String -> Id -> HTMLBuilder
idboxHTML kind i = divClass "idbox" $ do
   para $ do 
      font "id" $ ttText (showId i)
      spaces 3
      text $ "(" ++ kind ++ ")"
   unless (null $ description i) $
      para $ italic $ text (description i)

forStep :: Int -> (Id, Environment) -> HTMLBuilder  
forStep n (i, env) = do 
      spaces 3
      text "=>"
      space
      let target = up n ++ ruleFile i
          make | null (description i) = link target
               | otherwise = linkTitle target (description i)
      make (text (unqualified i))
      br
      unless (nullEnv env) $ do
         spaces 6
         text (show env)
         br

forTerm :: Exercise a -> Context a -> HTMLBuilder
forTerm ex ca = do
   text (prettyPrinterContext ex ca)
   br