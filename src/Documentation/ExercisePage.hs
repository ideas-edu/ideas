-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Documentation.ExercisePage (makeExercisePage, idboxHTML) where

import Common.Library hiding (up)
import Common.Utils (Some(..))
import Control.Monad
import Data.List
import Data.Maybe
import Documentation.DefaultPage
import Documentation.ExampleFile
import Documentation.OpenMathDerivations
import Documentation.RulePresenter
import Service.BasicServices
import Service.Diagnose
import Service.DomainReasoner
import Service.State
import Service.StrategyInfo
import System.Directory
import System.Random
import Text.HTML

makeExercisePage :: String -> Exercise a -> DomainReasoner ()
makeExercisePage dir ex = do
   let make     = makeId ex
       makeId a = generatePageAt (length (qualifiers a)) dir . ($ getId a)
       exFile   = dir ++ "/" ++ diagnosisExampleFile (getId ex)

   exampleFileExists <- liftIO (doesFileExist exFile)

   make exercisePageFile     (exercisePage exampleFileExists ex)
   make exerciseStrategyFile (strategyPage ex)
   unless (null (examples ex)) $ do
       make exerciseDerivationsFile (derivationsPage ex)
       liftIO $ makeOpenMathDerivations dir ex
   when exampleFileExists $ do
      ef <- liftIO (readExampleFile exFile)
      make exerciseDiagnosisFile (diagnosisPage ef ex)
    `catchError` \_ -> return ()

exercisePage :: Bool -> Exercise a -> HTMLBuilder
exercisePage exampleFileExists ex = do
   idboxHTML "exercise" exid

   h2 "1. General information"

   let bolds (x:xs) = bold x:xs
       bolds []     = []

   table False $ map bolds
      [ [ text "Code",   ttText (showId ex)]
      , [ text "Status", text (show $ status ex)]
      , [ text "Strategy"
        , link (up len ++ exerciseStrategyFile exid) $
             text (showId $ strategy ex)
        ]
      , [ text "OpenMath support"
        , text $ showBool $ isJust $ hasTermView ex
        ]
      {- , [ text "Textual feedback"
        , text $ showBool $ isJust $ getScript ex
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
   let rs   = rulesInStrategy (strategy ex)
       goUp = up (length (qualifiers ex))
       f r  = [ link (goUp ++ ruleFile r) $ ttText (showId r)
              , text $ showBool $ isBuggyRule r
              , text $ showBool $ not $ null $ getDescriptors r
              , text $ showBool $ r `elem` rs
              , when (isRewriteRule r) $
                   ruleToHTML (Some ex) r
              ]
   table True
      ( [ text "Rule name", text "Buggy", text "Args"
        , text "Used", text "Rewrite rule"
        ]
      : map f (ruleset ex)
      )
   when exampleFileExists $
      para $ link (up len ++ exerciseDiagnosisFile exid) $ do
         br
         text "See diagnosis examples"
   -- preText $ show $ treesToInfo ex trees

   h2 "3. Example"
   let state = generate (mkStdGen 0) ex Medium
   derivationHTML ex (stateTerm state)
   para $ unless (null (examples ex)) $
      link (up len ++ exerciseDerivationsFile exid) (text "More examples")
 where
   exid  = getId ex
   len   = length (qualifiers ex)
   {-
   trees = [ mapFirst getId (derivationTree (strategy ex) (inContext ex a))
           | (_, a) <- examples ex
           ] -}

strategyPage :: Exercise a -> HTMLBuilder
strategyPage ex = do
   h1 title
   h2 "1. Representation in XML"
   highlightXML True (strategyToXML (strategy ex))
   h2 "2. Locations"
   let f (loc, a) =
          [text (show loc), indent (length loc) >> text (showId a)]
       indent n = text (replicate (3*n) '.')
   table True
      ( [text "Location", text "Label"]
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
   when (isJust (hasTermView ex)) $
      let file = up upn ++ "derivations/" ++ showId ex ++ ".xml"
      in divClass "mathml" $ link file $ text "MathML"
   pre $ derivationM (forStep upn) (forTerm ex) der
   unless (ok der) $
      divClass "error" $ text "<<not ready>>"
 where
   upn = length (qualifiers ex)
   der = derivationPrevious (derivationDiffEnv (defaultDerivation ex a))
   ok  = maybe False (isReady ex) . fromContext . lastTerm

idboxHTML :: String -> Id -> HTMLBuilder
idboxHTML kind i = divClass "idbox" $ do
   divClass  "id-type" $ text kind
   spanClass "id-code" $ ttText (showId i)
   divClass  "id-description" $ text $
      if null (description i) then "no description" else description i

diagnosisPage :: ExampleFile -> Exercise a -> HTMLBuilder
diagnosisPage ef ex = do
   h1 ("Diagnosis examples for " ++ showId ex)
   let rs = [ (t, eb, descr) | Ready t eb descr <- items ef ]
   unless (null rs) $ table True $
      map text ["term", "ready", "description"] : map readyItem rs
   let ts = [ (t0, t1, expl) | Diagnose t0 t1 expl <- items ef ]
   zipWithM_ diagnoseItem [1::Int ..] ts
 where
   readyItem (t, eb, descr) =
      let mark = if ok then id else spanClass "error"
          (ok, result) =
             case parser ex t of
                Left _  -> (False, "error")
                Right a -> let b = isReady ex a
                           in (maybe True (==b) eb, showBool b)
      in map mark [ttText t, text result, text descr]

   diagnoseItem i (t0, t1, expl) = do
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

   getDiagnosis t0 t1 =
      case (parser ex t0, parser ex t1) of
         (Left msg, _) -> "parse error (before): " ++ msg
         (_, Left msg) -> "parse error (after): "  ++ msg
         (Right a, Right b) -> show (diagnose (emptyState ex a) b)

forStep :: Int -> ((Rule (Context a), Environment), Context a) -> HTMLBuilder
forStep n ((r, env), old) = do
      spaces 3
      text "=>"
      space
      let target = up n ++ ruleFile r
          make | null (description r) = link target
               | otherwise = titleA (description r) . link target
      make (text (unqualified r))
      let xs = expectedArguments r old
      unless (null xs) $ do
         br
         spaces 6
         text (intercalate ", " (map show xs))
      unless (nullEnv env) $ do
         br
         spaces 6
         text (show env)
      br

forTerm :: Exercise a -> Context a -> HTMLBuilder
forTerm ex ca = do
   text (prettyPrinterContext ex ca)
   br