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
module ExerciseInfoPanel (exerciseInfoPanel) where

import Graphics.UI.WX
import Common.Exercise
import Common.Id
import Data.Maybe
import Service.ExercisePackage
import Common.Utils (Some(..))
import Observable
import Control.Monad

exerciseInfoPanel :: Window a -> Control (Maybe (Some ExercisePackage)) -> IO (Panel ())
exerciseInfoPanel w ref = do
   p <- panel w []
   descr <- staticText p [font := fontDefault { _fontWeight = WeightBold} ]
   -- create text fields
   [txtCode, txtStatus, txtOpenMath, txtTextual, txtRestart,
       txtGenerator, txtExamples] <-
       replicateM 7 (staticText p [])
   -- layout
   set p [ layout := column 5 [hfill (widget descr), grid 10 5
      [ [ label "Code",                 hfill (widget txtCode)      ]
      , [ label "Status",               hfill (widget txtStatus)    ]
      , [ label "OpenMath support",     hfill (widget txtOpenMath)  ]
      -- , [ label "Textual feedback",     hfill (widget txtTextual)   ]
      , [ label "Restartable strategy", hfill (widget txtRestart)   ]
      , [ label "Exercise generator",   hfill (widget txtGenerator) ]
      , [ label "Examples",             hfill (widget txtExamples)  ]
      ]]]
   -- install event handler
   addObserver ref $ \mpkg -> 
      case mpkg of 
         Nothing -> do
            set descr [text := "No exercise selected"]
            let txts = [ txtCode, txtStatus, txtOpenMath, txtTextual
                       , txtRestart, txtGenerator, txtExamples ]
            forM_ txts $ \a -> set a [text := "-"]
         Just (Some pkg) -> do
            let ex = exercise pkg
            set descr        [text := description ex]
            set txtCode      [text := showId ex]
            set txtStatus    [text := show (status ex)]
            set txtOpenMath  [text := showBool (withOpenMath pkg)]
            -- set txtTextual   [text := showBool (isJust (getScript pkg))]
            set txtRestart   [text := showBool (canBeRestarted ex)]
            set txtGenerator [text := showBool (isJust (randomExercise ex))]
            set txtExamples  [text := show (length (examples ex))]
   return p

showBool :: Bool -> String
showBool b = if b then "yes" else "no"