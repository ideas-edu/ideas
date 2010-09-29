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
module Documentation.RulePage (makeRulePage) where

import Common.Context
import Common.Exercise
import Common.Transformation
import Common.Utils (commaList, Some(..))
import Control.Monad
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

makeRulePage :: String -> ExercisePackage a -> DomainReasoner ()
makeRulePage dir pkg = do
   let ex       = exercise pkg
       makeId a = generatePageAt (length (qualifiers a)) dir . ($ (getId a))
       exMap    = collectExamples ex

   forM_ (ruleset ex) $ \r ->
      makeId r ruleFile (rulePage ex exMap r)

{-
rulesPage :: Exercise a -> ExampleMap a -> HTMLBuilder
rulesPage ex exMap = do
   h1 title
   -- Groups
   let groups = sort (nub (concatMap ruleGroups (ruleset ex)))
   unless (null groups) $ do
      ul $ flip map groups $ \g -> do
         bold $ text $ g ++ ":"
         space
         let elems = filter ((g `elem`) . ruleGroups) (ruleset ex)
         text $ commaList $ map showId elems
      
   -- General info
   forM_ (zip [1..] (ruleset ex)) $ \(i, r) -> do
      h2 (show i ++ ". " ++ show r)
      rulePage ex exMap r
 where
   title = "Rules for " ++ showId ex
-}
rulePage :: Exercise a -> ExampleMap a -> Rule (Context a) -> HTMLBuilder
rulePage ex exMap r = do
   idboxHTML "rule" (getId r)
   para $ table 
      [ [bold $ text "Buggy", text $ showBool (isBuggyRule r)]
      , [bold $ text "Rewrite rule", text $ showBool (isRewriteRule r)]
      , [bold $ text "Groups", text $ commaList $ ruleGroups r]
      , [bold $ text "Siblings", text $ commaList $ map showId 
      $ ruleSiblings r] 
      ]
   when (isRewriteRule r) $ para $
      ruleToHTML (Some ex) r

   -- Examples
   let ys  = M.findWithDefault [] (getId r) exMap
       ups = length (qualifiers (getId r))
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