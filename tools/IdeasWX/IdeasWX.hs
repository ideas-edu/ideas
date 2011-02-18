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

import About
import ControlButtons
import DebugFrame
import Graphics.UI.WX
import Graphics.UI.WXCore
import NewAssignmentDialog
import Observable
import ProgressPanel
import Main.Options (shortVersion)
import Session
import System.Directory

main :: IO ()
main = start exerciseFrame

exerciseFrame :: IO ()
exerciseFrame = do 
   somePkg <- defaultAssignment
   session <- makeSession somePkg
   fontRef <- createControl (fontFixed {{- _fontFace = "Lucida Bright",-} _fontSize = 12})

   f <- frame [text := "IdeasWX Exercise Assistant", bgcolor := white]
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
   field2 <- statusField [statusWidth := 200, text := "version " ++ shortVersion]
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
   
   
   addObserver_ session $ do 
      -- update description
      descr <- currentDescription session
      set domainText [text := descr]
          
   addObserver_ session $ do
      -- update current
      txt <- currentText session
      set assignmentView [text := txt]
      set entryView [text := txt]
          
   addObserver_ session $ do
      -- update derivation 
      der <- derivationText session
      set derivationView [text := der]

   set feedbackView [text := "Welcome to the Exercise Assistant!"]
   notifyObservers session
   
   observeFont fontRef assignmentView
   observeFont fontRef entryView
   observeFont fontRef derivationView
   notifyObservers fontRef
      
   -- bind events 
   set newButton [on command := do
      txt <- suggestTerm session
      thisExercise txt session ]
 
   set changeButton [on command := newAssignmentDialog f session]

changeFont :: Window a -> Observable.Control FontStyle -> IO ()
changeFont w ref = do 
   f   <- getValue ref
   res <- fontDialog w f
   maybe (return ()) (setValue ref) res

observeFont :: (Textual a, Literate a) => Observable.Control FontStyle -> a -> IO Observable.Id
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