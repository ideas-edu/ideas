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
import Common.Context
import Common.Strategy hiding (not, replicate)
import Common.Transformation
import Service.ExerciseList 
import Service.TypedAbstractService hiding (exercise)
import Control.Monad
import Data.List
import Data.Maybe
import System.Random
import Text.HTML
import qualified Text.XML as XML
import Documentation.DefaultPage

makeExercisePage :: ExercisePackage a -> IO ()
makeExercisePage pkg = do
   let code = exerciseCode (exercise pkg)
   generatePage (exercisePageFile code) (exercisePage pkg)
   case derivationsPage pkg of 
      Nothing   -> return ()
      Just this ->
         generatePage (exerciseDerivationsFile code) this
      
exercisePage :: ExercisePackage a -> HTML
exercisePage pkg = defaultPage title 2 $ do
   h1 (description ex)
   table 
      [ [bold $ text "Code:",   ttText (show $ exerciseCode ex)]
      , [bold $ text "Status:", text (show $ status ex)]
      , [ bold $ text "OpenMath support"
        , text $ showBool $ withOpenMath pkg
        ]
      , [ bold $ text "Textual feedback"
        , text $ showBool $ isJust $ getExerciseText pkg
        ]
      ]
   
   h2 "1. Strategy"
   let f (loc, e)  = [text (show loc), indent (locationDepth loc) >> g e]
       g (Left a)  = text (strategyName a)
       g (Right a) = text (name a ++ " (rule)") 
       indent n    = text (replicate (3*n) '.')
   table ( [bold $ text "Location", bold $ text "Label"] 
         : map f (strategyLocations (strategy ex))
         )

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
   
   h2 "3. Example"
   let state = generateWith (mkStdGen 0) ex 5
   preText (showDerivation ex (fromContext $ context state))
   when (isJust (derivationsPage pkg)) $ 
      link (up 2 ++ exerciseDerivationsFile code) (text "More examples")
 where
   ex    = exercise pkg
   code  = exerciseCode ex
   title = "Exercise " ++ show (exerciseCode ex)
   
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