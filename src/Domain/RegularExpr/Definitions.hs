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
module Domain.RegularExpr.Definitions where

import Domain.RegularExpr.Expr
import Common.Uniplate
import Common.Utils (distinct)

deterministic :: Eq a => RE a -> Bool
deterministic regexp =
   distinct (lookahead regexp) && all deterministic (children regexp)

empty :: RE a -> Bool
empty = foldRE (False, True, const (False), const True, const True, id, (&&), (||))

lookahead :: RE a -> [a]
lookahead = map fst . firsts

firsts :: RE a -> [(a, RE a)]
firsts regexp =
   case regexp of
      EmptySet -> []
      Epsilon  -> []
      Atom a   -> [(a, Epsilon)]
      Option r -> firsts r
      Star r   -> firsts (nonempty r :*: Star r)
      Plus r   -> firsts (r :*: Star r)
      r :*: s  -> [ (a, q :*: s) | (a, q) <- firsts r ] ++
                  (if empty r then firsts s else [])
      r :|: s  -> firsts r ++ firsts s

nonempty :: RE a -> RE a
nonempty regexp = foldr (:|:) EmptySet [ Atom a :*: r | (a, r) <- firsts regexp ]