module Documentation.ExercisePage (makeExercisePage) where

import Common.Exercise
import Common.Context
import Common.Strategy hiding (not, replicate)
import Common.Transformation
import Service.ExerciseList
import Service.TypedAbstractService
import Control.Monad
import Data.List
import Data.Maybe
import System.Random
import Text.HTML
import Documentation.DefaultPage

makeExercisePage :: Exercise a -> IO ()
makeExercisePage ex = do
   generatePage (exercisePageFile ex) (exercisePage ex)
   case derivationsPage ex of 
      Nothing   -> return ()
      Just this ->
         generatePage (exerciseDerivationsFile ex) this
      

exercisePage :: Exercise a -> HTML
exercisePage ex = defaultPage title 2 $ do
   h1 (description ex)
   table 
      [ [bold $ text "Code:",   ttText (show $ exerciseCode ex)]
      , [bold $ text "Status:", text (show $ status ex)]
      , [ bold $ text "OpenMath support"
        , text $ showBool $ not $ null $ getOpenMathExercise (exerciseCode ex)
        ]
      ]
   
   h2 "1. Strategy"
   let f (loc, e)  = [text (show loc), indent (length loc) >> g e]
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
   when (isJust (derivationsPage ex)) $ 
      link (up 2 ++ exerciseDerivationsFile ex) (text "More examples")
 where
   title = "Exercise " ++ show (exerciseCode ex)
   
derivationsPage :: Exercise a -> Maybe HTML
derivationsPage ex = 
   case termGenerator ex of
      ExerciseList xs -> Just $ defaultPage title 2 $ do
         h1 "Examples"
         let f i x = do
                h2 (show i ++ ".")
                preText (showDerivation ex x)
         sequence_ $ zipWith f [1..] xs
      _ -> Nothing
 where
   title = "Derivations for " ++ show (exerciseCode ex)