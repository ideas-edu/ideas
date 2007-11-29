

{--------------------------------------------------- 
This is an interactive system in which a student can 
incrementally solve proposition formulae.

compile with

ghc -fglasgow-exts --make -package wx -o logsol LogicSolver.hs
/usr/local/wxhaskell/bin/macosx-app -v logsol

ghc -fglasgow-exts --make -package wx -i/Volumes/Work/Teaching/IPT/uust/lib/pretty -i/Volumes/Work/Teaching/IPT/uust/lib/scanner -i/Volumes/Work/Teaching/IPT/uust/lib/parsing -i/Volumes/Work/Teaching/IPT/uust/lib/util -o logsol LogicSolver.hs


Copyright (c)        2006 - 2007 

Johan Jeuring and Harrie Passier
---------------------------------------------------}

module Logic.Solver.LogicSolver where

-- Standard Haskell libraries
import List
import Maybe
import Time

-- wxHaskell libraries
import Graphics.UI.WX 
import Graphics.UI.WXCore

-- Equations model
-- import Logic.Solver.LogicDutchResources
import Logic.Solver.LogicEnglishResources  
import Logic.Solver.LogicFeedback 
import Logic.Solver.LogicGenerator
import Logic.Solver.LogicParser 


data LogicSolverState = LS{ formula  ::  String
                          , history  ::  [String]
                          }


main :: IO ()
main
  = start logicSolver

logicSolver :: IO ()
logicSolver  
  = do 
       -- the application frame
       f      <-  frame         [text := logic_solverText]                               

       -- create file menu  
       file   <-  menuPane      [text := fileText]  
       quit   <-  menuQuit file [help := quit_the_demoText, on command := close f]

       -- create Help menu
       hlp    <-  menuHelp      []
       about  <-  menuAbout hlp [help := about_logic_solverText]

       -- create New Formula menu
       newFormula <- menuPane [text := new_formulaText]
       new        <- menuItem newFormula [text := new_formula_ctrlKey
                                         , help := new_formulaText]
       -- create List of Rules 
       rulesMenu  <- menuPane [text := rewriteRulesText]  
       rulesItem  <- menuItem rulesMenu [ text := showRulesText
                                        , on command := menuHandler f list_of_rulesText ] 
       
       -- create statusbar field
       status <-  statusField   [text := welcome_to_Logic_SolverText]

       -- create the state
       v      <-  varCreate startState 
       s      <-  get v value

       -- create the different widgets
       labt   <-  staticText f [ text := working_areaText                        ]
       t      <-  textCtrl   f [ text := formula s                               ] -- formula to be submitted
       labh   <-  staticText f [ text := historyText                             ]
       h      <-  textCtrl   f [ text := concat (intersperse "\n" (history s))   ] -- formulae history
       labfb  <-  staticText f [ text := feedbackText                            ]
       fb     <-  textCtrl   f [ text := ""                                      ] -- feedback
       i      <-  staticText f [ text := ""                                      ] -- progression   

       set new [on command := newFormulaHandler t h fb i v] 
       
       
       -- create the Submit button
       sb <- button f [ text := submitText]
       set sb         [ on command := submitButtonHandler f t h fb i v]  

       -- create the Undo button
       ub <- button f [ text := undoText]
       set ub         [ on command := undoButtonHandler f t h fb i v]                                                      

       -- create the Hint button
       hb <- button f [ text := hintText]
       set hb         [ on command := hintButtonHandler fb v]                                                      

       -- create the Next step button
       nsb <- button f [ text := nextStepText ]
       set nsb         [ on command := nextstepButtonHandler fb v]                                                      

       -- create the Solved button
       sob <- button f [ text := solvedText ]
       set sob         [ on command := solvedButtonHandler {- f -} t {- h -}  fb {- i -}  v   ]  

       set f [ statusBar := [status]
             , menuBar   := [file, hlp, newFormula, rulesMenu]
             , on (menu about) := infoDialog f about_logic_solverText this_is_a_demo_of_the_Logic_SolverText
             , layout :=  margin 50 (column 10 [ widget labt
                                               , row 10 [ minsize (sz 300 115) $ widget t
                                                        , column 5 [ widget sb  -- submit
                                                                   , widget ub  -- undo
                                                                   , widget hb  -- hint
                                                                   , widget nsb -- next step
                                                                   , widget sob -- solved 
                                                                   ]
                                                        , stretch . halignRight $ widget i
                                                        ]
                                               , widget labh
                                               , row 10 [minsize (sz 300 200) $ widget h
                                                        ]
                                               , widget labfb
                                               , stretch . expand $ minsize (sz 600 200) $ widget fb
                                               ]
                                    )
             ]

        
 where startState =  LS{ formula  =  ""
                       , history  =  [""]
                       }
       
       menuHandler f txt = infoDialog f list_of_rewrite_rulesText txt         

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
 
          -- set i          [ text := indicatorsstring]
          
     
          if      not error
            then  set v [value := LS{ formula  =  entered
                                    , history  =  entered : history s
                                    } 
                        ]
            else  set v [value := LS{ formula  =  entered
                                    , history  =  history s
                                    } 
                        ]
          
       undoButtonHandler f t h fb i v = 
        do
          -- Get the state
          s <- get v value
         
          -- Get the submitted text
          entered <- get t text
           
          let (previousFormula, previousHistory) =  if entered /= safehead (history s) ""
                                                    then (safehead (history s) "" , history s)
                                                    else 
                                                    if length (history s) >1 
                                                    then ((history s) !! 1 , safetail (history s))
                                                    else (entered , history s)
                                                                       
          let (error,feedbackstring,indicatorsstring)  =  let  sth = safetail (history s)
                                                          in   if    not (null sth) 
                                                               then  feedback previousFormula (head sth)
                                                               else  feedback previousFormula previousFormula
          
          -- set i          [ text := indicatorsstring]

          set v [value := LS{ formula  =  previousFormula 
                            , history  =  previousHistory
                            } 
                ]           

          textCtrlSetValue t previousFormula   
          textCtrlSetValue h (concat (intersperse "\n------------------\n" previousHistory))
          textCtrlSetValue fb ""

          where  safetail x               =  if not (null x) then tail x else x
                 safehead x y             =  if not (null x) then head x else y
                 
                 
                  





       {-  ------------------------------------------------------------------------------------------
       undoButtonHandler f t h fb i v = 
        do
          -- Get the state
          s <- get v value
         
           
          let previousFormula    =  safehead (history s) ""                 
          let previousHistory    =  let  sth = safetail (history s)
                                    in   if    not (null sth) 
                                         then  sth
                                         else  history s
          
          let (error,feedbackstring,indicatorsstring)  =  let  sth = safetail (history s)
                                                          in   if    not (null sth) 
                                                               then  feedback previousFormula (head sth)
                                                               else  feedback previousFormula previousFormula
          
          -- set i          [ text := indicatorsstring]

          set v [value := LS{ formula  =  previousFormula 
                            , history  =  previousHistory
                            } 
                ]           

          textCtrlSetValue t previousFormula   
          textCtrlSetValue h (concat (intersperse "\n------------------\n" previousHistory))
          textCtrlSetValue fb ""

          where  safetail x    =  if not (null x) then tail x else x
                 safehead x y  =  if not (null x) then head x else y
    
       --------------------------------------------------------------------------------------------------- -}    
    
       hintButtonHandler fb v = 
        do
          -- Get the state
          s <- get v value
          -- textCtrlSetValue fb (hint (formula s)
          
          if      not (null (history s))
            -- then textCtrlSetValue fb (hint (formula s) )
            then textCtrlSetValue fb (hint (safehead (history s) "")) 
            else  textCtrlSetValue fb ""
          
          where  safehead x y  =  if not (null x) then head x else y  

       nextstepButtonHandler fb v = 
        do
          -- Get the state
          s <- get v value
          if      not (null (history s))
            -- then  textCtrlSetValue fb (nextstep (formula s) )
            then textCtrlSetValue fb (nextstep (safehead (history s) ""))
            else  textCtrlSetValue fb ""

          where  safehead x y  =  if not (null x) then head x else y 
      
       solvedButtonHandler {- f -} t {- h -}  fb {- i -}  v =
        do 
          -- Get the state
          s <- get v value

          -- Get the submitted text
          entered <- get t text
          
          -- Get the previous formula
          let previousFormula    =  safehead (history s) ""

          let (error,feedbackstring,indicatorsstring)  =  hasSolved entered previousFormula

          -- textCtrlSetValue t entered 
          
          textCtrlSetValue fb feedbackstring
 
          -- set i          [ text := indicatorsstring]
       
          where  safehead x y  =  if not (null x) then head x else y        


       newFormulaHandler t h fb i v =
        do
           time           <- getClockTime
           calendartime   <- toCalendarTime time
           let randomint  = getInt calendartime
           let  newForm   = generateFormula randomint 
           set v [value := LS{ formula  =  newForm
                             , history  =  [newForm]
                             }
                 ] 
           let (error,feedbackstring,indicatorsstring)  =  feedback newForm newForm
           textCtrlSetValue fb ""
           textCtrlSetValue t newForm
           textCtrlSetValue h newForm
           -- set i          [ text := indicatorsstring]
    
      
       getInt (CalendarTime year month day hour min sec psec wday yday tzname tz isdst)
                  = hour + min + sec + yday  
