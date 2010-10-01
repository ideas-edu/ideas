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

import Common.Context
import Common.Exercise
import Common.Derivation
import Common.Strategy hiding (not, replicate)
import Common.Transformation
import Common.Utils (Some(..), splitAtSequence)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Documentation.DefaultPage
import Documentation.RulePresenter
import Service.BasicServices
import Service.Diagnose
import Service.DomainReasoner
import Service.ExercisePackage
import Service.State
import Service.StrategyInfo
import System.Directory
import System.Random
import Text.HTML

makeExercisePage :: String -> ExercisePackage a -> DomainReasoner ()
makeExercisePage dir pkg = do
   let ex       = exercise pkg
       make     = makeId pkg
       makeId a = generatePageAt (length (qualifiers a)) dir . ($ (getId a))
       exFile   = dir ++ "/" ++ diagnosisExampleFile (getId ex)

   exampleFileExists <- liftIO (doesFileExist exFile)

   make exercisePageFile     (exercisePage exampleFileExists pkg)
   make exerciseStrategyFile (strategyPage ex)
   unless (null (examples (exercise pkg))) $
       make exerciseDerivationsFile (derivationsPage ex)
   when (exampleFileExists) $ do
      xs <- liftIO (readFile exFile)
      make exerciseDiagnosisFile (diagnosisPage xs pkg)
    `catchError` \_ -> return ()

exercisePage :: Bool -> ExercisePackage a -> HTMLBuilder
exercisePage exampleFileExists pkg = do
   idboxHTML "strategy" (getId pkg)
   
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
   
   para $ link (up level ++ exerciseStrategyFile exid) $
      text "See strategy details"

   h2 "2. Rules"
   let rs = rulesInStrategy (strategy ex)
       f r = [ text (showId r)
             , text $ showBool $ isBuggyRule r
             , text $ showBool $ hasArguments r
             , text $ showBool $ r `elem` rs
             , text $ concat $ intersperse "," (ruleGroups r)
             , when (isRewriteRule r) $
                  ruleToHTML (Some ex) r
             ]
   table ( [bold $ text "Rule name", bold $ text "Buggy"
           , bold $ text "Args" 
           , bold $ text "Used", bold $ text "Groups"
           , bold $ text "Rewrite rule"
           ]
         : map f (ruleset ex)
         )
   when exampleFileExists $ do
      para $ link (up level ++ exerciseDiagnosisFile exid) $ do
         br
         text "See diagnosis examples"

   h2 "3. Example"
   let state = generateWith (mkStdGen 0) pkg 5
   derivationHTML ex (term state)
   para $ unless (null (examples ex)) $ 
      link (up level ++ exerciseDerivationsFile exid) (text "More examples")
 where
   ex    = exercise pkg
   exid  = getId ex
   level = length (qualifiers pkg)

strategyPage :: Exercise a -> HTMLBuilder
strategyPage ex = do
   h1 title
   h2 "1. Representation in XML"
   highlightXML True (strategyToXML (strategy ex))
   h2 "2. Locations" 
   let f (loc, a) = 
          [text (show loc), indent (length loc) >> text (showId a)]
       indent n = text (replicate (3*n) '.')
   table ( [bold $ text "Location", bold $ text "Label"] 
         : map f (strategyLocations (strategy ex))
         )
 where
   title = "Strategy for " ++ showId ex

derivationsPage :: Exercise a -> HTMLBuilder
derivationsPage ex = do
   h1 "Examples"
   forM_ (zip [1 ..] (examples ex)) $ \(i, a) -> do
      h2 (show i ++ ".")
      derivationHTML ex a

derivationHTML :: Exercise a -> a -> HTMLBuilder
derivationHTML ex a = divClass "derivation" $ do 
   pre $ derivationM (forStep ups) (forTerm ex) der
   unless (ok der) $
      divClass "error" $ text "<<not ready>>"
 where
   ups = length (qualifiers ex)
   der = derivationDiffEnv (defaultDerivation ex a)
   ok  = maybe False (isReady ex) . fromContext . last . terms

idboxHTML :: String -> Id -> HTMLBuilder
idboxHTML kind i = divClass "idbox" $ do
   font "id" $ ttText (showId i)
   spaces 3
   text $ "(" ++ kind ++ ")"
   unless (null $ description i) $ do
      br
      italic (text (description i))

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
       
forStep :: HasId a => Int -> (a, Environment) -> HTMLBuilder  
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