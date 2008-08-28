-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- This module defines the Uniplate type class, and some utility functions. It
-- should be replaced in future by the original Uniplate library.
--
-----------------------------------------------------------------------------
module Common.Uniplate (
     -- * Uniplate type class and utility functions
     Uniplate(..), children, child, select, transform, transformAt, universe
   ) where
   
---------------------------------------------------------
-- Uniplate class for generic traversals

import Common.Utils (safeHead)
import Control.Monad

-- | The Uniplate type class offers some light-weight functions for generic traversals. Only
-- a minimal set of operations are supported
class Uniplate a where
   uniplate :: a -> ([a], [a] -> a)    -- ^ Function for generic traversals

-- | Returns all the immediate children of a term
children :: Uniplate a => a -> [a]
children = fst . uniplate

-- | Selects one immediate child of a term. Nothing indicates that the child does not exist
child :: Uniplate a => Int -> a -> Maybe a
child n = safeHead . drop n . children 
               
-- | Selects a child based on a path. Nothing indicates that the path is invalid
select :: Uniplate a => [Int] -> a -> Maybe a
select is a = foldM (flip child) a is

-- | Transforms one immediate child at a given index.
transform :: Uniplate a => Int -> (a -> a) -> a -> a
transform n f a = 
   let (as, build) = uniplate a 
       g i = if i==n then f else id
   in build (zipWith g [0..] as)

-- | Transforms one child based on a path.
transformAt :: Uniplate a => [Int] -> (a -> a) -> a -> a
transformAt is f = foldr transform f is

-- | Returns all subterms
universe :: Uniplate a => a -> [a]
universe a = a : [ c | b <- children a, c <- universe b ]