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

main :: IO ()
main = runAssignment dnfAssignment

runAssignment :: Assignment a -> IO ()
runAssignment assignment = 
    do  initGUI

        -- read Glade file (FIXME hardcoded path)
        windowXmlM <- xmlNew "bin/exerciseassistant.glade"
        let windowXml = case windowXmlM of
             Just windowXml -> windowXml
             Nothing -> error "Can't find the glade file \"exerciseassistant.glade\" in the bin subdirectory of the current directory"
        window         <- xmlGetWidget windowXml castToWindow   "window"
        assignmentView <- xmlGetWidget windowXml castToTextView "assignmentView"
        derivationView <- xmlGetWidget windowXml castToTextView "derivationView"
        entryView      <- xmlGetWidget windowXml castToTextView "entryView"
        feedbackView   <- xmlGetWidget windowXml castToTextView "feedbackView"
        readyButton    <- xmlGetWidget windowXml castToButton   "readyButton"
        hintButton     <- xmlGetWidget windowXml castToButton   "hintButton"
        stepButton     <- xmlGetWidget windowXml castToButton   "stepButton"
        undoButton     <- xmlGetWidget windowXml castToButton   "undoButton"
        submitButton   <- xmlGetWidget windowXml castToButton   "submitButton"

        -- get buffers from views
        assignmentBuffer <- textViewGetBuffer assignmentView 
        derivationBuffer <- textViewGetBuffer derivationView
        entryBuffer      <- textViewGetBuffer entryView 
        feedbackBuffer   <- textViewGetBuffer feedbackView 

        -- initialize assignment
        session <- newSession assignment
        initialAssignment <- currentTerm session
        textBufferSetText assignmentBuffer (prettyPrinter assignment $ initialAssignment)
        textBufferSetText entryBuffer (prettyPrinter assignment $ initialAssignment)
        textBufferSetText feedbackBuffer (show (stepsRemaining assignment initialAssignment) ++ " steps remaining")
        
        updateDerivation derivationBuffer session

        -- bind events
        onDelete window deleteEvent
        onDestroy window destroyEvent

        onClicked readyButton $ 
            do  textBufferSetText feedbackBuffer "ready"

        onClicked hintButton $
            do
                result     <- giveHintSession session
                case result of
                    (doc, rule) -> textBufferSetText feedbackBuffer (show rule ++ ";" ++ showDoc assignment doc)

        onClicked stepButton $
            do 
                result     <- giveStepSession session
                case result  of
                    (doc, subterm, newterm) -> 
                       textBufferSetText feedbackBuffer $ 
                       "Use " ++ showDoc assignment doc ++ "\nto rewrite subterm\n" ++ 
                       prettyPrinter assignment subterm ++ "\nresulting in\n" ++
                       prettyPrinter assignment newterm

        onClicked undoButton $
            do  txt1 <- get entryBuffer textBufferText
                txt2 <- get assignmentBuffer textBufferText
                if txt1 == txt2
                 then do
                   derivationUndo session
                   updateDerivation derivationBuffer session
                   textBufferSetText feedbackBuffer "undo"
                   term <- currentTerm session
                   textBufferSetText assignmentBuffer (prettyPrinter assignment term)
                   textBufferSetText entryBuffer (prettyPrinter assignment term)
                 else do
                   textBufferSetText entryBuffer txt2

        onClicked submitButton $
            do 
                txt        <- get entryBuffer textBufferText
                result     <- feedbackSession session txt
                case result of
                   SyntaxError doc msug -> 
                      textBufferSetText feedbackBuffer $ 
                         showDoc assignment doc ++ maybe "" (\a -> "\nDid you mean " ++ prettyPrinter assignment a) msug
                   Incorrect doc msug ->
                      textBufferSetText feedbackBuffer $ 
                         showDoc assignment doc ++ maybe "" (\a -> "\nDid you mean " ++ prettyPrinter assignment a) msug
                   Correct doc singleRule -> do
                      let new = either undefined id $ parser assignment txt -- REWRITE !
                      textBufferSetText feedbackBuffer $
                         showDoc assignment doc ++ "\n" ++ 
                            if isJust singleRule 
                            then show (stepsRemaining assignment new) ++ " steps remaining"
                            else "unknown rule"
                      when (isJust singleRule) $ do
                        textBufferSetText assignmentBuffer txt
                        derivationStep session (fromJust singleRule) new
                        updateDerivation derivationBuffer session
        
        -- show widgets and run GUI
        widgetShowAll window
        mainGUI

deleteEvent :: Event -> IO Bool
deleteEvent = const (return False)

destroyEvent :: IO ()
destroyEvent = do mainQuit

updateDerivation :: TextBufferClass w => w -> Session a -> IO ()
updateDerivation buffer session = do
   txt <- getDerivationText session
   textBufferSetText buffer txt