-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
--  $Id: Sequential.hs 6612 2014-06-12 07:57:59Z bastiaan $

module Ideas.Common.Strategy.Derived
   ( filterP, hide
   , fromAtoms, Sym(..), atomic, concurrent, (<@>)
   ) where

import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Process

useFirst :: Choice f => (a -> Process a -> f b) -> f b -> Process a -> f b
useFirst op e = onMenu (step e op) . menu

filterP :: (a -> Bool) -> Process a -> Process a
filterP cond = fold (\a q -> if cond a then a ~> q else empty) done

hide :: (a -> Bool) -> Process a -> Process a
hide cond = fold (\a q -> if cond a then a ~> q else q) done

data Sym a = Single a | Composed (Process a)

fromAtoms :: Process (Sym a) -> Process a
fromAtoms = fold f done
 where
   f (Single a)   = (a ~>)
   f (Composed p) = (p <*>)

atomic :: IsProcess f => f (Sym a) -> f (Sym a)
atomic = single . Composed . fromAtoms . toProcess

concurrent :: IsProcess f => (a -> Bool) -> f a -> f a -> f a
concurrent switch x y = normal (toProcess x) (toProcess y)
 where
   normal p q = stepBoth q p <|> (stepRight q p <|> stepRight p q)

   stepBoth  = useFirst stop2 . useFirst stop2 done
   stop2 _ _ = empty

   stepRight p = useFirst op empty
    where
      op a = (a ~>) . (if switch a then normal else stepRight) p

-- Alternate combinator
(<@>) :: IsProcess f => f a -> f a -> f a
p0 <@> q0 = rec (toProcess q0) (toProcess p0)
 where
   rec q  = useFirst (\a r -> a ~> rec r q) (bothOk q)
   bothOk = useFirst (\_ _ -> empty) done

---------------------------------------------------------------------------