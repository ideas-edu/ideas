{-# OPTIONS -fglasgow-exts #-}
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
module Main (main) where

-- import Configuration
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.List
import System.Directory
import Session
import Service.Options (versionText)
import Observable
import About
import NewAssignmentDialog
import ControlButtons
import ProgressPanel
import Data.Maybe
import qualified Service.TypedAbstractService as TAS
import Common.Strategy (prefixToSteps)

title :: String
title = "IdeasWX Exercise Assistant"

main :: IO ()
main = start exerciseFrame

exerciseFrame :: IO ()
exerciseFrame = do 
   -- cfg <- readConfig
   session <- makeSession defaultAssignment
   fontRef <- createControl (fontFixed {{- _fontFace = "Lucida Bright",-} _fontSize = 12})

   f <- frame [text := title, bgcolor := white]
   bmp <- iconCreateFromFile "ideas.ico" sizeNull
   topLevelWindowSetIcon f bmp
   
   -- Menu bar
   mfile  <- menuPane [text := "&File"]
   mexit  <- menuItem mfile [text := "&Exit", help := "Quit the application"] 
   mview  <- menuPane [text := "&View"]
   mfont  <- menuItem mview [text := "&Change font", help := "Change font settings for expressions"]
   mdebug <- menuItem mview [text := "&Debug frame", help := "Open a frame for debugging"]
   mhelp  <- menuPane [text := "&Help"]
   mabout <- menuItem mhelp [text := "&About", help := "About Ideas"]
   set f [ menuBar := [mfile, mview, mhelp]
         , on (menu mexit)  := close f
         , on (menu mabout) := aboutIdeas f 
         , on (menu mdebug) := debugFrame session
         , on (menu mfont)  := changeFont f fontRef
         ] 
         
   -- Status bar
   field1 <- statusField []
   field2 <- statusField [statusWidth := 200, text := versionText]
   set f [statusBar := [field1,field2]] 
   
   -- Left Panel
   leftPanel <- panel f []
   
   -- Domain Panel
   domainPanel  <- panel leftPanel []
   domainText   <- staticText leftPanel []
   newButton    <- button domainPanel [text := "New exercise"]
   changeButton <- button domainPanel [text := "Change exercise"]
   set domainPanel [layout := row 10 [hfill $ widget domainText, hglue, widget newButton, widget changeButton]]
      
   assignmentView <- textCtrlRich leftPanel [bgcolor := myGrey]
   entryView      <- textCtrlRich leftPanel [bgcolor := myGrey]
   
   -- Right Panel
   rightPanel     <- panel f []
   derivationView <- textCtrlRich rightPanel [bgcolor := myGrey] 
   feedbackView   <- textCtrlRich rightPanel [bgcolor := myGrey]
   
   progress <- progressPanel rightPanel session
   
   imagePanel     <- panel rightPanel [bgcolor := white]
   mfull <- searchPath "ounl.jpg"
   case mfull of 
      Just full -> do imageOUNL <- imageCreateFromFile full
                      set imagePanel [on paint := \dc _ -> drawImage dc imageOUNL (pt 0 0) [], size := sz 233 86]
      Nothing   -> return ()
   
   bp <- controlButtons leftPanel session 
            (get entryView text)
            (\s -> set feedbackView [text := s])
   
   set leftPanel [layout := column 10
      [ hfill $ widget domainPanel, hstretch $ label "Assignment", fill $ widget assignmentView
      , hstretch $ label "Enter modified term", fill $ widget entryView
      , row 0 [hglue, widget bp, hglue]]]
      
   
   set rightPanel [layout := column 10 
      [ hstretch $ label "Derivation", fill $ widget derivationView
      , fill (widget progress)
      , hstretch $ label "Feedback", fill $ widget feedbackView
      , row 0 [hglue, widget imagePanel]]]
   
   set f [ layout := margin 20 $ row 30 [fill $ widget leftPanel, fill $ widget rightPanel] 
         , size := sz 800 600]

   mapM_ (flip textCtrlSetEditable False) [assignmentView, derivationView, feedbackView]

   -- initialize exercise
   
   let updateDescription = do
          descr <- currentDescription session
          set domainText [text := descr]
          
   let updateCurrent = do 
          txt <- currentText session
          set assignmentView [text := txt]
          set entryView [text := txt]
          
   let updateDerivation = do
          der <- derivationText session
          set derivationView [text := der]

    

   set feedbackView [text := "Welcome to the Exercise Assistant!"]
   
   addObserver_ session updateDescription
   addObserver_ session updateCurrent
   addObserver_ session updateDerivation
   notifyObservers session
   
   observeFont fontRef assignmentView
   observeFont fontRef entryView
   observeFont fontRef derivationView
   notifyObservers fontRef
      
   -- bind events 
   set newButton [on command := do
      txt <- suggestTerm 5 session -- TODO: fix difficulty!!
      thisExercise txt session ]
 
   set changeButton [on command := newAssignmentDialog f session]

debugFrame :: Session -> IO ()
debugFrame session = do
   f <- frame [text := "Debug", bgcolor := white]
   rulebox <- singleListBox  f []
   b   <- button f [text := "Apply", on command := do
             n <- get rulebox selection
             nextStep session n
             return ()]
   txt <- staticText f [text := "(no rules)"]
   
   stp <- singleListBox f []
   
   set f [layout := column 10 [row 10 [fill (widget rulebox), widget b, widget txt], fill (widget stp)], size := sz 400 200]
   
   let execObserver f = addObserver session f >> getValue session >>= f
   
   execObserver $ \(Some st) -> do
      let result = TAS.allfirsts (currentState (getDerivation st))
          rs     = [ show r ++ " @ " ++ show p | (r, p, _) <- concat result ]
          msg    = case length rs of
                      0 -> "(no rules)"
                      1 -> "(1 rule)"
                      n -> "(" ++ show n ++ " rules)"
      set rulebox [items := rs, selection := 0]
      set txt [text := msg]
   execObserver $ \(Some st) -> do
      let xs = maybe [] (map show . prefixToSteps) (TAS.prefix (currentState (getDerivation st)))
      set stp [items := xs]

changeFont :: Window a -> Observable.Control FontStyle -> IO ()
changeFont w ref = do 
   f   <- getValue ref
   res <- fontDialog w f
   maybe (return ()) (setValue ref) res

observeFont :: (Textual a, Literate a) => Observable.Control FontStyle -> a -> IO ()
observeFont ref a = addObserver ref $ \new -> do
   s <- get a text
   set a [font := new, text := s]

searchPath :: String -> IO (Maybe String)
searchPath filename = rec paths
 where
   paths  = [".", "bin"]
   rec [] = return Nothing
   rec (p:ps) = do
      let full = p ++ "/" ++ filename
      b <- doesFileExist full
      if b then return (Just full) else rec ps
      
myGrey = rgb 230 230 230