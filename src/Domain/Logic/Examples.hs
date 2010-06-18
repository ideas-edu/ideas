-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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


import Domain.Logic.Formula
import Common.Utils (ShowString(..))





exampleProofs :: [(SLogic, SLogic)]
exampleProofs = [(Not(p :||: (Not (p) :&&: q)), Not(p :||: q)),
                ((p :->:q):||: Not (p), (p :->: q) :||: q),
                ((p :&&: Not(q)):||:(q :&&: Not(p)), (p :||:q):&&:Not(p :&&: q)),
                (Not(p :||: Not(p :||: Not (q))), Not(p :||: q)),
                (p :<->: q, (p :->: q) :&&: (q :->: p)),
                ((p :&&: q) :->: p, T),
                ((p :->: q) :||: (q :->: p), T),
                ((q :->: (Not(p) :->: q)) :->: p, Not(p) :->: (q :&&: ((p :&&: q) :&&: q))),
                ((p :->: Not(q)):->:q, (s :||:(s :->:(q :||: p))) :&&: q),
                (p :->: (q :->: r), (p :->: q) :->: (p :->:r)),
                (Not((p :->: q) :->: Not(q :->: p)), (p :<->: q)),
                 (((p :->: q):->: (p :->: s)), (Not (q) :->: Not(p)) :->: (Not(s) :->: Not(p)))]
 where
   p = Var (ShowString "p")
   q = Var (ShowString "q")
   s = Var (ShowString "s")
   r = Var (ShowString "r")



