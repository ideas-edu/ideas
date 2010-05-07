-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland and Universiteit Utrecht.
-- This file is distributed  under the terms of the GNU General Public 
-- License. For more information, see the file "LICENSE.txt", which is 
-- included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  johanj@cs.uu.nl
-- Stability   :  provisional
-- Portability :  unknown
--
-----------------------------------------------------------------------------

module Domain.Programming.Strategies99
   ( lastStrategy
   ) where

import Common.Strategy
import Domain.Programming.PreludeS
import Domain.Programming.Helium -- for definition of Module
import Prelude hiding (sequence)

--------------------------------------------------------------------------------
-- Problem 1 in the 99 questions list.
--------------------------------------------------------------------------------

-- | A strategy for constructing last, called myLast to avoid clashes with
-- | the prelude function of the same name. 
lastStrategy  :: LabeledStrategy Module
lastStrategy  =  label "myLast :: [a] -> a" 
       $  lastRecurS 
      <|> lastFoldr1S 
      <|> lastReverseS

lastRecurS  :: LabeledStrategy Module
lastRecurS  =  
     label "explicit recursive definition of myLast"
  $  progS [declFunS [ funS (lhsS "myLast" [patConS "[x]"]) 
                            (rhsS (varS "x") [])
                     , funS (lhsS "myLast" [patInfixConS (patConS "_") 
                                                         ":" 
                                                         (patS "xs")
                                           ]
                            )
                            (rhsS (varS "myLast" # [varS "xs"]) [])
                     ]
           ]
           
-- Questions:
-- 1 what is the difference between patConS, patS, and patInficConS?
-- 2 what does the empty list do at the end of rhsS? 
-- 3 shouldn't I tell somewhere that x is a name? Use patS "x" instead? But then
--   I have to construct a noninfix pattern consisting of a number of 
--   components, how do I do that?

lastFoldr1S  :: LabeledStrategy Module
lastFoldr1S  = 
     label "definition of myLast using foldr1"
  $  progS [declPatS "myLast" (rhsS (foldr1S consS) [])]
       where consS = exprParenS $ constS idS
  
lastReverseS  :: LabeledStrategy Module
lastReverseS  = 
     label "definition of myLast using reverse"
  $  progS [declPatS "myLast" (rhsS (compS headS reverseS) [])]

--------------------------------------------------------------------------------
-- Problem 2 in the 99 questions list.
--------------------------------------------------------------------------------

-- | A strategy for constructing the function butLast, which returns the last 
-- | but one element of a list.
butLastS  :: LabeledStrategy Module
butLastS  =  label "butLast :: [a] -> a"
          $  butLastInitS
         <|> butLastReverseS
         <|> butLastRecur1S
         <|> butLastRecur2S         
         
butLastInitS     :: LabeledStrategy Module
butLastInitS     =  
     (label "definition of butLast using init")
  $  progS [declPatS "butLast" (rhsS (compS lastS initS) [])]

butLastReverseS  :: LabeledStrategy Module
butLastReverseS  = 
     (label "definition of butLast using reverse")
  $  progS [declFunS [ funS (lhsS "butLast" [patConS "x"])
                            (rhsS (opS "!!" 
                                       (Just (varS "reverse" # [varS "x"]))
                                       (Just (intS "1"))
                                  ) 
                                  []
                            )
                     ]
           ]

butLastRecur1S   :: LabeledStrategy Module
butLastRecur1S   = undefined

butLastRecur2S   :: LabeledStrategy Module
butLastRecur2S   = undefined        
