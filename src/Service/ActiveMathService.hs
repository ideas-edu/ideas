-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Service.ActiveMathService where

import Service.AbstractService
import Common.Context
import Common.Utils
import Data.Maybe

thd :: (a,b,c) -> c
thd (a,b,c) = c -- if location and ruleid are relevant thd = id

-- | Get final result in the given context
getResult :: State -> Expression
getResult = thd . last . derivation

-- | Get the next step
getNextStep :: State -> State
getNextStep = thd . onefirst

-- | Get the (list of) concepts (rules) that have to applied in the step
getConcepts :: State -> [RuleID]
getConcepts = map (\(a,b,c)->a) . allfirsts

-- | Get the value of correctness of the user input
getCorrectness :: State -> Expression -> Bool
getCorrectness s e = isOk $ submit s e
                     where
                       isOk (Ok _ _) = True
                       isOk _        = False

-- | Get list of misconceptions (buggy) rules
getMisconceptions :: State -> Expression -> [RuleID]
getMisconceptions s e = buggyIDs $ submit s e 
                        where
                          buggyIDs (Buggy ids) = ids
                          buggyIDs _           = []

-- | Get all correct solution paths
getExpertSolutionPaths :: State -> [[(RuleID, Location, State)]]
getExpertSolutionPaths s = allPaths ("",[],s)
                           where 
                             allPaths s = let fsts = allfirsts $ thd s
                                          in case fsts of
                                                  [] -> [[s]]
                                                  x  -> map (s:) $ concatMap allPaths x

-- | Get one default solution path
getExpertSolutionPath :: State -> [(RuleID, Location, State)]
getExpertSolutionPath = fromMaybe (error "Expert solution path") . safeHead . getExpertSolutionPaths

-- | Get the path the user used to come to current state
getUserSolutionPath :: State -> [(RuleID, Location, Expression)]
getUserSolutionPath = derivation
