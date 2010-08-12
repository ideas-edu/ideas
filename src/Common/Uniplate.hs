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
-- Exports a subset of Data.Generics.Uniplate
--
-----------------------------------------------------------------------------
module Common.Uniplate
   ( -- * Uniplate type class and utility functions
     Uniplate(..), universe, children
   , transform, transformM, descend, rewrite, rewriteM
   ) where

import Data.Generics.Uniplate

{-
---------------------------------------------------------
-- Uniplate class for generic traversals

import Common.Utils (safeHead)
import Control.Monad
-- | The Uniplate type class offers some light-weight functions for generic traversals. Only
-- a minimal set of operations are supported
class Uniplate a where
   uniplate :: a -> ([a], [a] -> a)    -- ^ Function for generic traversals

-- | Returns all subterms
universe :: Uniplate a => a -> [a]
universe a = a : [ c | b <- children a, c <- universe b ]

-- | Returns all the immediate children of a term
children :: Uniplate a => a -> [a]
children = fst . uniplate

-- | A bottom-up transformation
transform :: Uniplate a => (a -> a) -> a -> a
transform g a = g $ f $ map (transform g) cs
 where
   (cs, f) = uniplate a

-- | Monadic variant of transform
transformM :: (Monad m, Uniplate a) => (a -> m a) -> a -> m a
transformM g a = mapM (transformM g) cs >>= (g . f)
 where
   (cs, f) = uniplate a

-- | Applies a function to its immediate children
descend :: Uniplate a => (a -> a) -> a -> a
descend g a = 
   let (cs, f) = uniplate a
   in f (map g cs)

-- | Applies the function at a position until this is no longer possible
rewrite :: Uniplate a => (a -> Maybe a) -> a -> a
rewrite f = transform g
    where g x = maybe x (rewrite f) (f x)

-- | Monadic variant of rewrite
rewriteM :: (Monad m, Uniplate a) => (a -> m (Maybe a)) -> a -> m a
rewriteM f = transformM g
    where g x = f x >>= maybe (return x) (rewriteM f)

---------------------------------------------------------
-- Additional functions

-- | Like universe, but also returns the location of the subterm
subtermsAt :: Uniplate a => a -> [([Int], a)]
subtermsAt a = ([], a) : [ (i:is, b) | (i, c) <- zip [0..] (children a), (is, b) <- subtermsAt c ]
         
-- | Selects a child based on a path. Nothing indicates that the path is invalid
getTermAt :: Uniplate a => [Int] -> a -> Maybe a
getTermAt is a = foldM (flip child) a is
 where
   child n = safeHead . drop n . children 

-- | Apply a function to one immediate child.
applyTo :: Uniplate a => Int -> (a -> a) -> a -> a
applyTo n f a = 
   let (as, build) = uniplate a 
       g i = if i==n then f else id
   in build (zipWith g [0..] as)

-- | Monadic variant of applyTo
applyToM :: (Monad m, Uniplate a) => Int -> (a -> m a) -> a -> m a
applyToM n f a = 
   let (as, build) = uniplate a 
       g (i, b) = if i==n then f b else return b
   in liftM build $ mapM g (zip [0..] as)

-- | Apply a function at a given position (based on a path).
applyAt :: Uniplate a => [Int] -> (a -> a) -> a -> a
applyAt is f = foldr applyTo f is

-- | Monadic variant of applyAt
applyAtM :: (Monad m, Uniplate a) => [Int] -> (a -> m a) -> a -> m a
applyAtM is f = foldr applyToM f is

somewhere :: Uniplate a => (a -> a) -> a -> [a]
somewhere f = somewhereM (return . f)

somewhereM :: (MonadPlus m, Uniplate a) => (a -> m a) -> a -> m a
somewhereM f a = msum $ f a : map g [0..n-1]
 where 
   n   = length (children a)
   g i = applyToM i (somewhereM f) a
-}