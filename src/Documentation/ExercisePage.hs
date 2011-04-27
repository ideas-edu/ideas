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

import Common.Utils (Some(..), splitAtSequence, commaList)
import Common.Library hiding (up)
import Control.Monad
import Data.Char
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

   let bolds (x:xs) = bold x:xs
       bolds []     = []

   table $ map bolds
      [ [ text "Code",   ttText (showId ex)]
      , [ text "Status", text (show $ status ex)]
      , [ text "Strategy"
        , link (up len ++ exerciseStrategyFile exid) $
             text (showId $ strategy ex)
        ]
      , [ text "OpenMath support"
        , text $ showBool $ withOpenMath pkg
        ]
      {- , [ text "Textual feedback"
        , text $ showBool $ isJust $ getScript pkg
        ] -}
      , [ text "Restartable strategy"
        , text $ showBool $ canBeRestarted ex
        ] 
      , [ text "Exercise generator"
        , text $ showBool $ isJust $ randomExercise ex
        ]
      , [ text "Examples"
        , text $ show $ length $ examples ex
        ]
      ]

   h2 "2. Rules"
   let rs  = rulesInStrategy (strategy ex)
       ups = up (length (qualifiers pkg)) 
       f r = [ link (ups ++ ruleFile r) $ ttText (showId r)
             , text $ showBool $ isBuggyRule r
             , text $ showBool $ hasArguments r
             , text $ showBool $ r `elem` rs
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
      para $ link (up len ++ exerciseDiagnosisFile exid) $ do
         br
         text "See diagnosis examples"
   -- preText $ show $ treesToInfo ex trees

   h2 "3. Example"
   let state = generate (mkStdGen 0) pkg Medium
   derivationHTML ex (stateTerm state)
   para $ unless (null (examples ex)) $ 
      link (up len ++ exerciseDerivationsFile exid) (text "More examples")
 where
   ex    = exercise pkg
   exid  = getId ex
   len   = length (qualifiers pkg)
   trees = [ mapFirst getId (derivationTree (strategy ex) (inContext ex a)) 
           | (_, a) <- examples ex 
           ]

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
   forM_ (zip [1::Int ..] (examples ex)) $ \(i, (_, a)) -> do
      h2 (show i ++ ".")
      derivationHTML ex a

derivationHTML :: Exercise a -> a -> HTMLBuilder
derivationHTML ex a = divClass "derivation" $ do 
   pre $ derivationM (forStep ups) (forTerm ex) der
   unless (ok der) $
      divClass "error" $ text "<<not ready>>"
 where
   ups = length (qualifiers ex)
   der = derivationPrevious (derivationDiffEnv (defaultDerivation ex a))
   ok  = maybe False (isReady ex) . fromContext . lastTerm

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
   forM_ (zip [1::Int ..] (mapMaybe f (lines xs))) $ \(i, (t0, t1, expl)) -> do 
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
       
forStep :: Int -> ((Rule (Context a), Environment), Context a) -> HTMLBuilder  
forStep n ((r, env), old) = do 
      spaces 3
      text "=>"
      space
      let target = up n ++ ruleFile r
          make | null (description r) = link target
               | otherwise = linkTitle target (description r)
      make (text (unqualified r))
      let xs = fromMaybe [] (expectedArguments r old)
          g (ArgValue descr x) = labelArgument descr ++ "=" ++ showArgument descr x
      unless (null xs) $ do
         br
         spaces 6
         text (commaList (map g xs))
      unless (nullEnv env) $ do
         br 
         spaces 6
         text (show env)
      br

forTerm :: Exercise a -> Context a -> HTMLBuilder
forTerm ex ca = do
   text (prettyPrinterContext ex ca)
   br