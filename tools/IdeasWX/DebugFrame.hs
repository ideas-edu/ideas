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
module DebugFrame (debugFrame) where

import Graphics.UI.WX
import Session
import Observable
import Service.TypedAbstractService
import Common.Strategy

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
      let result = allfirsts (currentState (getDerivation st))
          rs     = [ show r ++ " @ " ++ show p | (r, p, _) <- concat result ]
          msg    = case length rs of
                      0 -> "(no rules)"
                      1 -> "(1 rule)"
                      n -> "(" ++ show n ++ " rules)"
      set rulebox [items := rs, selection := 0]
      set txt [text := msg]
   execObserver $ \(Some st) -> do
      let xs = maybe [] (map show . prefixToSteps) (prefix (currentState (getDerivation st)))
      set stp [items := xs]