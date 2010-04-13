-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Documentation.ExercisePage (makeExercisePage) where

import Common.Exercise
import Common.Strategy hiding (not, replicate)
import Common.Transformation
import Service.ExercisePackage
import Service.TypedAbstractService hiding (exercise)
import Control.Monad
import Data.List
import Common.Utils (commaList, Some(..))
import Data.Maybe
import System.Random
import qualified Data.Map as M
import Service.RulesInfo (rewriteRuleToFMP, collectExamples)
import Text.HTML
import Text.OpenMath.Object
import Text.OpenMath.FMP
import qualified Text.XML as XML
import Documentation.DefaultPage

makeExercisePage :: String -> ExercisePackage a -> IO ()
makeExercisePage dir pkg = do
   let code = exerciseCode (exercise pkg)
   generatePage dir (exercisePageFile code) (exercisePage pkg)
   generatePage dir (exerciseStrategyFile code) (strategyPage pkg)
   generatePage dir (exerciseRulesFile code) (rulesPage pkg)
   case derivationsPage pkg of 
      Nothing   -> return ()
      Just this ->
         generatePage dir (exerciseDerivationsFile code) this
      
exercisePage :: ExercisePackage a -> HTML
exercisePage pkg = defaultPage title 2 $ do
   h1 (description ex)
   
   h2 "1. General information"
   table 
      [ [bold $ text "Code",   ttText (show $ exerciseCode ex)]
      , [bold $ text "Status", text (show $ status ex)]
      , [ bold $ text "OpenMath support"
        , text $ showBool $ withOpenMath pkg
        ]
      , [ bold $ text "Textual feedback"
        , text $ showBool $ isJust $ getExerciseText pkg
        ]
      , [ bold $ text "Restartable strategy"
        , text $ showBool $ canBeRestarted ex
        ] 
      , [ bold $ text "Exercise generator"
        , text $ showBool $ isJust $ randomExercise ex
        ]
      , [ bold $ text "Examples"
        , text $ show $ length $ examples ex
        ]
      ]
   
   para $ link (up 2 ++ exerciseStrategyFile code) $
      text "See strategy details"

   h2 "2. Rules"
   let rs = rulesInStrategy (strategy ex)
       f r = [ text (name r)
             , text $ showBool $ isBuggyRule r
             , text $ showBool $ hasArguments r
             , text $ showBool $ r `elem` rs
             , text $ concat $ intersperse "," (ruleGroups r)
             , when (isRewriteRule r) $
                  image (ruleImageFileHere ex r)
             ]
   table ( [bold $ text "Rule name", bold $ text "Buggy"
           , bold $ text "Args" 
           , bold $ text "Used", bold $ text "Groups"
           , bold $ text "Rewrite rule"
           ]
         : map f (ruleset ex)
         )
   para $ link (up 2 ++ exerciseRulesFile code) $
      text "See rule details"
   
   
   h2 "3. Example"
   let state = generateWith (mkStdGen 0) ex 5
   preText (showDerivation ex (term state))
   when (isJust (derivationsPage pkg)) $ 
      link (up 2 ++ exerciseDerivationsFile code) (text "More examples")
 where
   ex    = exercise pkg
   code  = exerciseCode ex
   title = "Exercise " ++ show (exerciseCode ex)

strategyPage :: ExercisePackage a -> HTML
strategyPage pkg = defaultPage title 2 $ do
   h1 title
   h2 "1. Representation in XML"
   preText (XML.showXML (XML.toXML (strategy ex)))
   h2 "2. Locations" 
   let f (loc, e)  = [text (show loc), indent (locationDepth loc) >> g e]
       g (Left a)  = text (strategyName a)
       g (Right a) = text (name a ++ " (rule)") 
       indent n    = text (replicate (3*n) '.')
   table ( [bold $ text "Location", bold $ text "Label"] 
         : map f (strategyLocations (strategy ex))
         )
 where
   ex    = exercise pkg
   code  = exerciseCode ex
   title = "Strategy for " ++ show code

rulesPage :: ExercisePackage a -> HTML
rulesPage pkg = defaultPage title 2 $ do
   h1 title
   -- Groups
   let groups = sort (nub (concatMap ruleGroups (ruleset ex)))
   unless (null groups) $ do
      ul $ flip map groups $ \g -> do
         bold $ text $ g ++ ":"
         space
         let elems = filter ((g `elem`) . ruleGroups) (ruleset ex)
         text $ commaList $ map name elems
      
   -- General info
   forM_ (zip [1..] (ruleset ex)) $ \(i, r) -> do
      h2 (show i ++ ". " ++ show r)
      para $ text (ruleDescription r)
      para $ table 
         [ [bold $ text "Buggy", text $ showBool (isBuggyRule r)]
         , [bold $ text "Rewrite rule", text $ showBool (isRewriteRule r)]
         , [bold $ text "Groups", text $ commaList $ ruleGroups r]
         , [bold $ text "Siblings", text $ commaList $ ruleSiblings r] 
         ]
      when (isRewriteRule r) $ para $
         image (ruleImageFileHere ex r)
      -- Examples
      let ys = M.findWithDefault [] (name r) exampleMap
      unless (null ys) $ do
         h3 "Examples"
         forM_ (take 3 ys) $ \(a, b) -> para $ tt $ 
            preText $ prettyPrinter ex a ++ "\n   =>\n" ++ prettyPrinter ex b
         
      -- FMPS
      let xs = getRewriteRules r
      unless (null xs) $ do
         h3 "Formal Mathematical Properties"
         forM_ xs $ \(Some rr, b) -> para $ do
            let fmp = rewriteRuleToFMP b rr
            ttText $ show $ XML.makeXML "FMP" $ 
               XML.builder (omobj2xml (toObject fmp))
 where
   ex    = exercise pkg
   code  = exerciseCode ex
   title = "Strategy for " ++ show code
   exampleMap = collectExamples ex

derivationsPage :: ExercisePackage a -> Maybe HTML
derivationsPage pkg
   | null xs   = Nothing
   | otherwise = Just $ defaultPage title 2 $ do
        unless (errs==0) $ 
           errorLine $ preText $ "Warning: " ++ show errs ++ " example(s) with an incorrect derivation"
        h1 "Examples"
        forM_ (zip [1 ..] ds) $ \(i, d) -> do
            h2 (show i ++ ".")
            preText d
 where
   ex    = exercise pkg
   code  = exerciseCode ex
   title = "Derivations for " ++ show code
   xs    = examples ex
   ds    = map (showDerivation ex) xs
   errs  = let p s =  "<<no derivation>>" `isSuffixOf` s 
                   || "<<not ready>>" `isSuffixOf` s
           in length $ filter p ds
   
errorLine :: HTMLBuilder -> HTMLBuilder
errorLine b = XML.element "font" $ do
   "color" XML..=. "red"
   bold b