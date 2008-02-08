{--------------------------------------------------- 
This is an interactive system in which a student can 
incrementally solve proposition formulae.

Copyright (c)        2006 - 2007 

Johan Jeuring, Harrie Passier, Bastiaan Heeren, Alex Gerdes
and Arthur van Leeuwen
---------------------------------------------------}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Data.Maybe
import Session
import Common.Transformation
import Common.Strategy
import Domain.Logic
import Control.Monad
import Domain.Fraction
import Domain.LinearAlgebra
import Domain.RelationAlgebra (cnfAssignment)

domains :: [PackedAssignment]
domains = [ Pack dnfAssignment, Pack reduceMatrixAssignment, Pack opgave6b
          , Pack solveSystemAssignment, Pack simplAssignment, Pack cnfAssignment
          ]

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
        applyButton    <- fromXml castToButton      "applyButton"
        undoButton     <- fromXml castToButton      "undoButton"
        submitButton   <- fromXml castToButton      "submitButton"
        newButton      <- fromXml castToButton      "newButton"
        domainBox      <- fromXml castToComboBox    "domainBox"
        progressBar    <- fromXml castToProgressBar "progressBar"
        progressLabel  <- fromXml castToLabel       "progressLabel"

        mapM_ (\(Pack a) -> comboBoxAppendText domainBox (shortTitle a)) domains
        comboBoxSetActive  domainBox 0

        -- get buffers from views
        assignmentBuffer <- textViewGetBuffer assignmentView 
        derivationBuffer <- textViewGetBuffer derivationView
        entryBuffer      <- textViewGetBuffer entryView 
        feedbackBuffer   <- textViewGetBuffer feedbackView 

        -- initialize assignment
        session <- makeSession (head domains)
        
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
        updateAll

        -- bind events
        onDelete  window $ \_ -> return False
        onDestroy window mainQuit

        onChanged domainBox $ do
           index <- comboBoxGetActive domainBox
           newAssignment (domains !! fromMaybe 0 index) session
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
        
        onClicked applyButton $ do
           (txt, ok) <- applyStep session
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
        
        -- show widgets and run GUI
        widgetShowAll window
        mainGUI