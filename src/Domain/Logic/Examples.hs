-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  josje.lodder@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A set of example proofs
--
-----------------------------------------------------------------------------
module Domain.Logic.Examples
   ( exampleProofs
   ) where

import Common.Exercise
import Common.Utils (ShowString(..))
import Domain.Logic.Formula

exampleProofs :: [(Difficulty, (SLogic, SLogic))]
exampleProofs = 
   [ {-  1 -} difficult $ (Not(p :||: (Not p :&&: q)), Not(p :||: q))
   , {-  2 -} difficult $ ((p :->:q):||: Not p, (p :->: q) :||: q)
   , {-  3 -} ok        $ ((p :&&: Not q):||:(q :&&: Not p), (p :||:q):&&:Not(p :&&: q))
   , {-  4 -} difficult $ (Not(p :||: Not(p :||: Not q)), Not(p :||: q))
   , {-  5 -} ok        $ (p :<->: q, (p :->: q) :&&: (q :->: p))
   , {-  6 -} difficult $ ((p :&&: q) :->: p, T)
   , {-  7 -} ok        $ ((p :->: q) :||: (q :->: p), T)
   , {-  8 -} difficult $ ((q :->: (Not p :->: q)) :->: p, Not p :->: (q :&&: ((p :&&: q) :&&: q)))
   , {-  9 -} ok        $ ((p :->: Not q):->:q, (s :||:(s :->:(q :||: p))) :&&: q)
   , {- 10 -} difficult $ (p :->: (q :->: r), (p :->: q) :->: (p :->:r))
   , {- 11 -} ok        $ (Not((p :->: q) :->: Not(q :->: p)), p :<->: q)
   , {- 12 -} ok        $ ((p :->: q):->: (p :->: s), (Not q :->: Not p) :->: (Not s :->: Not p))
   , {- 13 -} difficult $ (Not((p :->:q) :->: (p:&&:q)), (p :->: q) :&&: (Not p :||: Not q))
   , {- 14 -} difficult $ (Not((p :<->: q) :->: (p :||: (p :<->: q))), F)
   , {- 15 -} difficult $ (q :&&: p, p :&&: (q :||: q))
   ]

 where
   difficult x = (Difficult, x)
   ok x = (Medium, x)
 
   p = Var (ShowString "p")
   q = Var (ShowString "q")
   s = Var (ShowString "s")
   r = Var (ShowString "r")

{-
makeTestCases :: IO ()
makeTestCases = zipWithM_ makeTestCase [0..] exampleProofs
   
makeTestCase :: Int -> (SLogic, SLogic) -> IO ()
makeTestCase n (p, q) = 
   writeFile ("proof" ++ show n ++ ".json")  
      (json $ show p ++ " == " ++ show q)
   
json :: String -> String
json s = unlines
   [ "{ \"method\" : \"derivation\""
   , ", \"params\" : [[\"logic.proof\", \"[]\", " ++ show s ++ ", \"\"]]"
   , ", \"id\"     : 42"
   , "}"
   ] -}