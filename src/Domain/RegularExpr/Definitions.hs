-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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
import Common.Utils.Uniplate
import Common.Utils (distinct)

deterministic :: (Show a, Eq a) => RE a -> Bool
deterministic = deterministicSimple {-
   case (deterministicSimple r, det r) of
      (b1, b2) | b1==b2 -> b1
      _ -> error $ show r -}
       
deterministicSimple :: Eq a => RE a -> Bool
deterministicSimple regexp =
   distinct (lookahead regexp) && all deterministicSimple (children regexp)

det :: Eq a => RE a -> Bool
det regexp =
   case regexp of
      EmptySet -> True
      Epsilon  -> True
      Atom _   -> True
      Option r -> det (r :|: Epsilon)
      Star r   -> det r
      Plus r   -> det (r :*: Star r)
      r :|: s  -> lookahead r `disj` lookahead s && det r && det s
      EmptySet  :*: r -> det r
      Epsilon   :*: r -> det r
      Atom _    :*: r -> det r
      Option s  :*: r -> det ((s :|: Epsilon) :*: r)
      Star s    :*: r -> lookahead s `disj` lookahead r && det s && det r
      Plus s    :*: r -> det ((s :*: Star s) :*: r)
      (q :|: s) :*: r -> det ((q :*: r) :|: (s :*: r))
      (q :*: s) :*: r -> det (q :*: (s :*: r))


disj xs = all (`notElem` xs)

empty :: RE a -> Bool
empty = foldRE (False, True, const False, const True, const True, id, (&&), (||))

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