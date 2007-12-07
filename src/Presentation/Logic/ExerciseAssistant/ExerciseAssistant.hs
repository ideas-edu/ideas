{--------------------------------------------------- 
This is an interactive system in which a student can 
incrementally solve proposition formulae.

Copyright (c)        2006 - 2007 

Johan Jeuring, Harrie Passier, Bastiaan Heeren, Alex Gerdes
and Arthur van Leeuwen
---------------------------------------------------}

module Main where

-- GTK2Hs Imports
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

-- To keep the state and pass it to the event handlers (that are in IO)
import Data.IORef

-- Equations model
import Common.Assignment
import Common.Transformation
import Common.Strategy
import Domain.Logic

import Control.Monad

main :: IO ()
main =
    do  initGUI

        -- read Glade file (FIXME hardcoded path)
        windowXmlM <- xmlNew "bin/exerciseassistant.glade"
        let windowXml = case windowXmlM of
             (Just windowXml) -> windowXml
             Nothing -> error "Can't find the glade file \"exerciseassistant.glade\" in the bin subdirectory of the current directory"
        window <- xmlGetWidget windowXml castToWindow "window"
        assignmentView <- xmlGetWidget windowXml castToTextView "assignmentView"
        derivationView <- xmlGetWidget windowXml castToTextView "derivationView"
        entryView <- xmlGetWidget windowXml castToTextView "entryView"
        feedbackView <- xmlGetWidget windowXml castToTextView "feedbackView"
        readyButton <- xmlGetWidget windowXml castToButton "readyButton"
        hintButton <- xmlGetWidget windowXml castToButton "hintButton"
        stepButton <- xmlGetWidget windowXml castToButton "stepButton"
        undoButton <- xmlGetWidget windowXml castToButton "undoButton"
        submitButton <- xmlGetWidget windowXml castToButton "submitButton"

        -- get buffers from views
        assignmentBuffer <- textViewGetBuffer assignmentView 
        derivationBuffer <- textViewGetBuffer derivationView
        entryBuffer <- textViewGetBuffer entryView 
        feedbackBuffer <- textViewGetBuffer feedbackView 

        -- initialize assignment
        initialAssignment <- randomTerm dnfAssignment
        textBufferSetText assignmentBuffer (prettyPrinter dnfAssignment $ initialAssignment)
        textBufferSetText entryBuffer (prettyPrinter dnfAssignment $ initialAssignment)

        assignmentState <- newIORef initialAssignment

        -- bind events
        onDelete window deleteEvent
        onDestroy window destroyEvent

        onClicked readyButton $ 
            do  textBufferSetText feedbackBuffer "ready"

        onClicked hintButton $
            do
                currentAssignment <- readIORef assignmentState
                case giveHint dnfAssignment currentAssignment of
                    (doc, rule) -> textBufferSetText feedbackBuffer (show rule ++ ";" ++ show doc)

        onClicked stepButton $
            do 
                currentAssignment <- readIORef assignmentState
                case giveStep dnfAssignment currentAssignment of
                    (doc, subterm, newterm) -> 
                       textBufferSetText feedbackBuffer $ 
                       "Use " ++ show doc ++ "\nto rewrite subterm\n" ++ 
                       prettyPrinter dnfAssignment subterm ++ "\nresulting in\n" ++
                       prettyPrinter dnfAssignment newterm

        onClicked undoButton $
            do 
                currentAssignment <- readIORef assignmentState
                textBufferSetText feedbackBuffer "undo"

        onClicked submitButton $
            do 
                currentAssignment <- readIORef assignmentState
                txt <- get entryBuffer textBufferText
                case feedback dnfAssignment currentAssignment txt of
                   SyntaxError doc msug -> 
                      textBufferSetText feedbackBuffer $ 
                         show doc ++ maybe "" (\a -> "\nDid you mean " ++ prettyPrinter dnfAssignment a) msug
                   Incorrect doc msug ->
                      textBufferSetText feedbackBuffer $ 
                         show doc ++ maybe "" (\a -> "\nDid you mean " ++ prettyPrinter dnfAssignment a) msug
                   Correct doc ok -> do
                      textBufferSetText feedbackBuffer $
                         show doc ++ "\n" ++ if ok then "ok" else "unknown rule"
                      when ok $ do
                        textBufferSetText assignmentBuffer txt
                        writeIORef assignmentState (either undefined id $ parser dnfAssignment txt) -- REWRITE !
        
        -- show widgets and run GUI
        widgetShowAll window
        mainGUI

deleteEvent :: Event -> IO Bool
deleteEvent = const (return False)

destroyEvent :: IO ()
destroyEvent = do mainQuit