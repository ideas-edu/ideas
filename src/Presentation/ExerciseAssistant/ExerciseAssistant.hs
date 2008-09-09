-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Data.Maybe
import Session
import SupplyArguments
import Common.Transformation
import Control.Monad
import qualified Domain.LinearAlgebra as LA
import Service.ExerciseList
import Data.IORef

exercises :: [Some Exercise]
exercises = Some LA.opgave6b : exerciseList

main :: IO ()
main = 
    do  initGUI

        -- read Glade file (FIXME hardcoded path)
        windowXmlM <- xmlNew "bin/exerciseassistant.glade"
        let fromXml f = xmlGetWidget windowXml f
            windowXml = case windowXmlM of
             Just windowXml -> windowXml
             Nothing -> error "Can't find the glade file \"exerciseassistant.glade\" in the bin subdirectory of the current directory"
            
        window         <- fromXml castToWindow      "window"
        assignmentView <- fromXml castToTextView    "assignmentView"
        derivationView <- fromXml castToTextView    "derivationView"
        entryView      <- fromXml castToTextView    "entryView"
        feedbackView   <- fromXml castToTextView    "feedbackView"
        readyButton    <- fromXml castToButton      "readyButton"
        hintButton     <- fromXml castToButton      "hintButton"
        stepButton     <- fromXml castToButton      "stepButton"
        nextButton     <- fromXml castToButton      "nextButton"
        undoButton     <- fromXml castToButton      "undoButton"
        submitButton   <- fromXml castToButton      "submitButton"
        newButton      <- fromXml castToButton      "newButton"
        domainBox      <- fromXml castToComboBox    "domainBox"
        ruleBox        <- fromXml castToComboBox    "ruleBox"
        progressBar    <- fromXml castToProgressBar "progressBar"
        progressLabel  <- fromXml castToLabel       "progressLabel"
        imageOUNL      <- fromXml castToImage       "imageOUNL"

        imageSetFromFile imageOUNL "bin/ounl.jpg"
        let lightBlue = Color (235*256) (244*256) (255*256)
            -- ligthGrey = Color (230*256) (230*256) (230*256)
        widgetModifyBg window StateNormal lightBlue
        
        
        -- flip mapM_ [assignmentView, derivationView, feedbackView] $ \w -> 
        --    widgetModifyBase w StateNormal ligthGrey
        
        mapM_ (\(Some a) -> comboBoxAppendText domainBox (shortTitle a)) exercises
        comboBoxSetActive  domainBox 0

        -- get buffers from views
        assignmentBuffer <- textViewGetBuffer assignmentView 
        derivationBuffer <- textViewGetBuffer derivationView
        entryBuffer      <- textViewGetBuffer entryView 
        feedbackBuffer   <- textViewGetBuffer feedbackView 

        -- initialize exercise
        session <- makeSession (head exercises)
        
        let fillRuleBox = do
               -- first clear the box
               result <- comboBoxGetModel ruleBox
               case result of 
                  Just model -> listStoreClear (castToListStore model)
                  Nothing    -> return ()
               -- then fill the box again
               names <- ruleNames session
               mapM_ (comboBoxAppendText ruleBox) names
           
        let updateAll = do
               txt <- currentText session
               textBufferSetText assignmentBuffer txt
               textBufferSetText entryBuffer txt
               der <- derivationText session
               textBufferSetText derivationBuffer der
               (x, y) <- progressPair session
               labelSetText progressLabel (show x ++ "/" ++ show y)
               progressBarSetFraction progressBar (if y==0 then 1 else fromIntegral x / fromIntegral y)

        textBufferSetText feedbackBuffer "Welcome to the Exercise Assistant!"
        fillRuleBox
        updateAll

        -- bind events
        onDelete  window $ \_ -> return False
        onDestroy window mainQuit

        onChanged domainBox $ do
           index <- comboBoxGetActive domainBox
           newExercise (exercises !! fromMaybe 0 index) session
           fillRuleBox
           updateAll

        onClicked newButton $ do
           newTerm session
           updateAll

        onClicked readyButton $ do
           txt <- readyText session
           textBufferSetText feedbackBuffer txt
           
        onClicked hintButton $ do
           txt <- hintText session
           textBufferSetText feedbackBuffer txt
           
        onClicked stepButton $ do
           txt <- stepText session
           textBufferSetText feedbackBuffer txt
        
        onClicked nextButton $ do
           (txt, ok) <- nextStep session
           textBufferSetText feedbackBuffer txt
           when ok updateAll
        
        onClicked undoButton $ do
           txt1 <- get entryBuffer textBufferText
           txt2 <- get assignmentBuffer textBufferText
           if txt1 == txt2 
              then undo session >> updateAll
              else textBufferSetText entryBuffer txt2
           
        onClicked submitButton $ do
           cur       <- get entryBuffer textBufferText
           (txt, ok) <- submitText cur session
           textBufferSetText feedbackBuffer txt
           when ok updateAll

        onChanged ruleBox $ do
           (iterBegin, iterEnd) <- textBufferGetSelectionBounds entryBuffer
           posBegin <- textIterGetOffset iterBegin
           posEnd   <- textIterGetOffset iterEnd
           cur      <- get entryBuffer textBufferText
           mloc     <- subTermAtIndices cur posBegin posEnd session
           mi       <- comboBoxGetActive ruleBox

           when (isJust mi) $ do
              comboBoxSetActive ruleBox (-1)
              Some rule <- getRuleAtIndex (fromJust mi) session
              case (mloc, hasArguments rule) of
                 (Nothing, _) | posBegin /= posEnd ->
                    textBufferSetText feedbackBuffer "Invalid selection: not a subterm"
                 
                 (_, False) -> do
                    (txt, ok) <- applyRuleAtIndex (fromJust mi) mloc [] session
                    textBufferSetText feedbackBuffer txt
                    when ok updateAll
                    
                 _ -> do
                    ref <- newIORef Nothing 
                    w   <- argumentWindow ref rule
                    onDestroy w $ do
                       result <- readIORef ref
                       case result of 
                          Nothing -> return ()
                          Just list -> do
                             (txt, ok) <- applyRuleAtIndex (fromJust mi) mloc list session
                             textBufferSetText feedbackBuffer txt
                             when ok updateAll
                    return ()
        
        -- show widgets and run GUI
        widgetShowAll window
        mainGUI