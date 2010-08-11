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
module Documentation.ExercisePage (makeExercisePage) where

import Common.Exercise
import Common.Strategy hiding (not, replicate)
import Common.Transformation
import Common.Utils (commaList, Some(..), splitAtSequence)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Documentation.DefaultPage
import Service.BasicServices
import Service.Diagnose
import Service.DomainReasoner
import Service.ExercisePackage
import Service.RulesInfo (rewriteRuleToFMP, collectExamples)
import Service.State
import Service.StrategyInfo
import System.Directory
import System.Random
import Text.HTML
import Text.OpenMath.FMP
import Text.OpenMath.Object
import qualified Data.Map as M
import qualified Text.XML as XML

makeExercisePage :: String -> ExercisePackage a -> DomainReasoner ()
makeExercisePage dir pkg = do
   let ex     = exercise pkg
       make   = generatePageAt 2 dir . ($ (getId pkg))
       exFile = dir ++ "/" ++ diagnosisExampleFile (getId ex)

   exampleFileExists <- liftIO (doesFileExist exFile)

   make exercisePageFile     (exercisePage exampleFileExists pkg)
   make exerciseStrategyFile (strategyPage ex)
   make exerciseRulesFile    (rulesPage ex)
   unless (null (examples (exercise pkg))) $
       make exerciseDerivationsFile (derivationsPage ex)
   when (exampleFileExists) $ do
      xs <- liftIO (readFile exFile)
      make exerciseDiagnosisFile (diagnosisPage xs pkg)
    `catchError` \_ -> return ()

exercisePage :: Bool -> ExercisePackage a -> HTMLBuilder
exercisePage exampleFileExists pkg = do
   h1 (description ex)
   
   h2 "1. General information"
   table 
      [ [bold $ text "Code",   ttText (showId ex)]
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
   
   para $ link (up 2 ++ exerciseStrategyFile exid) $
      text "See strategy details"

   h2 "2. Rules"
   let rs = rulesInStrategy (strategy ex)
       f r = [ text (showId r)
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
   para $ do
      link (up 2 ++ exerciseRulesFile exid) $
         text "See rule details"
      when exampleFileExists $ do
         link (up 2 ++ exerciseDiagnosisFile exid) $ do
            br
            text "See diagnosis examples"

   h2 "3. Example"
   let state = generateWith (mkStdGen 0) pkg 5
   preText (showDerivation ex (term state))
   unless (null (examples ex)) $ 
      link (up 2 ++ exerciseDerivationsFile exid) (text "More examples")
 where
   ex   = exercise pkg
   exid = getId ex

strategyPage :: Exercise a -> HTMLBuilder
strategyPage ex = do
   h1 title
   h2 "1. Representation in XML"
   highlightXML True (strategyToXML (strategy ex))
   h2 "2. Locations" 
   let f (loc, a) = 
          [text (show loc), indent (locationDepth loc) >> text (showId a)]
       indent n = text (replicate (3*n) '.')
   table ( [bold $ text "Location", bold $ text "Label"] 
         : map f (strategyLocations (strategy ex))
         )
 where
   title = "Strategy for " ++ showId ex

rulesPage :: Exercise a -> HTMLBuilder
rulesPage ex = do
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
      para $ text (description r)
      para $ table 
         [ [bold $ text "Buggy", text $ showBool (isBuggyRule r)]
         , [bold $ text "Rewrite rule", text $ showBool (isRewriteRule r)]
         , [bold $ text "Groups", text $ commaList $ ruleGroups r]
         , [bold $ text "Siblings", text $ commaList $ map showId 
         $ ruleSiblings r] 
         ]
      when (isRewriteRule r) $ para $
         image (ruleImageFileHere ex r)
      -- Examples
      let ys = M.findWithDefault [] (getId r) exampleMap
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
            highlightXML False $ XML.makeXML "FMP" $ 
               XML.builder (omobj2xml (toObject fmp))
 where
   title = "Rules for " ++ showId ex
   exampleMap = collectExamples ex

derivationsPage :: Exercise a -> HTMLBuilder
derivationsPage ex = do
   unless (errs==0) $ 
      errorLine $ preText $ "Warning: " ++ show errs ++ " example(s) with an incorrect derivation"
   h1 "Examples"
   forM_ (zip [1 ..] ds) $ \(i, d) -> do
      h2 (show i ++ ".")
      preText d
 where
   ds   = map (showDerivation ex) (examples ex)
   errs = let p s =  "<<no derivation>>" `isSuffixOf` s 
                  || "<<not ready>>" `isSuffixOf` s
          in length $ filter p ds

diagnosisPage :: String -> ExercisePackage a -> HTMLBuilder
diagnosisPage xs pkg = do
   h1 ("Diagnosis examples for " ++ showId pkg)
   forM_ (zip [1..] (mapMaybe f (lines xs))) $ \(i, (t0, t1, expl)) -> do 
      h2 (show i ++ ".")
      preText (t0 ++ "\n  =>\n" ++ t1)
      para $ do
         unless (null expl) $ do 
            bold $ text "Description:"
            space
            text expl
            br
            bold $ text "Diagnosis:"
            space
            text (getDiagnosis t0 t1)
 where
   ex  = exercise pkg
   f a = do 
      (x, b) <- splitAtSequence "==>" a
      let (y, z) = fromMaybe (b, "") (splitAtSequence ":::" b)
          trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
      return (trim x, trim y, trim z)
      
   getDiagnosis t0 t1 = 
      case (parser ex t0, parser ex t1) of
         (Left msg, _) -> "parse error (before): " ++ msg
         (_, Left msg) -> "parse error (afterr): " ++ msg
         (Right a, Right b) -> show (diagnose (emptyState pkg a) b)
   
errorLine :: HTMLBuilder -> HTMLBuilder
errorLine b = XML.element "font" $ do
   "color" XML..=. "red"
   bold b