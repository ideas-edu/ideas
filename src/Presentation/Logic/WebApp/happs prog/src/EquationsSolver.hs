{--------------------------------------------------- 
This is an interactive system in which a student can 
incrementally solve linear equations.

compile with

ghc -fglasgow-exts --make -package wx -o eser EquationsSolver.hs
/usr/local/wxhaskell/bin/macosx-app -v eser




Copyright (c)        2005 - 2006 
Johan Jeuring and Harrie Passier
---------------------------------------------------}
module Main where

-- Standard Haskell libraries
import List
import Maybe
import Time

-- wxHaskell libraries
import Graphics.UI.WX 
import Graphics.UI.WXCore

-- Equations model
import EquationsPretty (equations2string) -- Remove! Move to equations generator
import EquationsParser (parseEquations) -- Remove! Move to equations generator
import EquationsEnglishResources  
import EquationsFeedback (feedback, hint)
import EquationsGenerator (generateEqs)
import EquationsUtility (second)

data EquationsSolverState = ES{ equations  ::  String
                              , history    ::  [String]
                              }

-- "x+2*y = 5\n2*y-z = 2*x-1\nz = x+y"

main :: IO ()
main
  = start equationsSolver

equationsSolver :: IO ()
equationsSolver  
  = do 
       -- the application frame
       f      <-  frame         [text := equation_solverText]                               

       -- create file menu  
       file   <-  menuPane      [text := fileText]
       quit   <-  menuQuit file [help := quit_the_demoText, on command := close f]

       -- create Help menu
       hlp    <-  menuHelp      []
       about  <-  menuAbout hlp [help := about_equation_solverText]

       -- create statusbar field
       status <-  statusField   [text := welcome_to_Equation_SolverText]

       -- create the state
       v      <-  varCreate startState 
       s      <-  get v value

       -- create the different widgets
       labt   <-  staticText f [ text := working_areaText                        ]
       t      <-  textCtrl   f [ text := equations s                             ] -- equations to be submitted
       labh   <-  staticText f [ text := historyText                             ]
       h      <-  textCtrl   f [ text := concat (intersperse "\n" (history s))   ] -- equations history
       labfb  <-  staticText f [ text := feedbackText                            ]
       fb     <-  textCtrl   f [ text := ""                                      ] -- feedback
       i      <-  staticText f [ text := ""                                      ] -- progression   

       -- create the Submit button
       sb <- button f [ text := submitText]
       set sb         [ on command := submitButtonHandler f t h fb i v]  

       -- create the Undo button
       ub <- button f [ text := undoText]
       set ub         [ on command := undoButtonHandler f t h fb i v]                                                      

       -- create the Hint button
       hb <- button f [ text := hintText]
       set hb         [ on command := hintButtonHandler fb v]                                                      

       -- create the "Generate two new equantions" button
       gb2<- button f [ text := two_new_eqs ]
       set gb2        [on command := new2EqsHandler t h fb i v]  

       -- create the "Generate three new equantions" button
       gb3<- button f [ text := three_new_eqs]  
       set gb3        [on command := new3EqsHandler t h fb i v]  



       set f [ statusBar := [status]
             , menuBar   := [file,hlp]
             , on (menu about) := infoDialog f about_equation_solverText this_is_a_demo_of_the_Equation_SolverText
             , layout :=  margin 50 (column 10 [ widget labt
                                               , row 10 [ minsize (sz 200 115) $ widget t
                                                        , column 5 [ widget sb
                                                                   , widget ub
                                                                   , widget hb
                                                                   , widget gb2
                                                                   , widget gb3
                                                                   ]
                                                        , stretch . halignRight $ widget i
                                                        ]
                                               , widget labh
                                               , row 10 [minsize (sz 200 200) $ widget h
                                                        ]
                                               , widget labfb
                                               , stretch . expand $ minsize (sz 600 200) $ widget fb
                                               ]
                                    )
             ]

 where startState =  ES{ equations  =  ""
                       , history    =  [""]
                       }
       
                

       submitButtonHandler f t h fb i v = 
        do 
          -- Get the state
          s <- get v value

          -- Get the submitted text
          entered <- get t text

          let (error,feedbackstring,indicatorsstring)  =  feedback entered (head (history s)) 

          textCtrlSetValue t entered 
          
          
          if      not error
            then  textCtrlSetValue h (concat (intersperse "\n------------------\n" (entered : history s)))
            else  textCtrlSetValue h (concat (intersperse "\n------------------\n" (history s)))
          
          textCtrlSetValue fb feedbackstring
 
          set i          [ text := indicatorsstring]
          
     
          if      not error
            then  set v [value := ES{ equations  =  entered
                                    , history    =  entered : history s
                                    } 
                        ]
            else  set v [value := ES{ equations  =  entered
                                    , history    =  history s
                                    } 
                        ]
          

       undoButtonHandler f t h fb i v = 
        do
          -- Get the state
          s <- get v value
         
           
          let previousEquations  =  safehead (history s) ""           -- nog oplossen
          let previousHistory    =  let  sth = safetail  (history s)
                                    in   if    not (null sth) 
                                         then  sth
                                         else  history s
          

          set v [value := ES{ equations  =  previousEquations 
                            , history    =  previousHistory
                            } 
                ]                              

          textCtrlSetValue t previousEquations   
          textCtrlSetValue h (concat (intersperse "\n------------------\n" previousHistory))
          textCtrlSetValue fb ""
          
          let (error,feedbackstring,indicatorsstring)  =  let  sth = safetail (history s)
                                                          in   if    not (null sth) 
                                                               then  feedback previousEquations (head sth) 
                                                               else  feedback previousEquations previousEquations 
          
          set i          [ text := indicatorsstring]
          where  safetail x    =  if not (null x) then tail x else x
                 safehead x y  =  if not (null x) then head x else y
    

       hintButtonHandler fb v = 
        do
          -- Get the state
          s <- get v value
          if      not (null (history s))
            then  textCtrlSetValue fb (hint (head (history s)))
            else  textCtrlSetValue fb ""
          


       new2EqsHandler t h fb i v =
        do
           time           <- getClockTime
           calendartime   <- toCalendarTime time
           let randomint  = getInt calendartime
           let  newEqs    = equations2string (parseEquations (generateEqs 2 5 randomint))
           set v [value := ES{ equations  =  newEqs
                             , history    =  [newEqs]
                             }
                 ] 
           let (error,feedbackstring,indicatorsstring)  =  feedback newEqs newEqs
           textCtrlSetValue fb ""
           textCtrlSetValue t newEqs
           textCtrlSetValue h newEqs
           set i          [ text := indicatorsstring]
    



       new3EqsHandler t h fb i v =
        do
           time           <- getClockTime
           calendartime   <- toCalendarTime time
           let randomint  = getInt calendartime
           let  newEqs    = equations2string (parseEquations (generateEqs 3 5 randomint))  
           set v [value := ES{ equations  =  newEqs
                             , history    =  [newEqs]
                             }
                 ] 
           let (error,feedbackstring,indicatorsstring)  =  feedback newEqs newEqs
           textCtrlSetValue fb ""
           textCtrlSetValue t newEqs
           textCtrlSetValue h newEqs
           set i          [ text := indicatorsstring]



       getInt (CalendarTime year month day hour min sec psec wday yday tzname tz isdst)
                  = hour + min + sec + yday  
