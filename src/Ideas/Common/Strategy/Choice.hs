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
--  $Id: Sequential.hs 6598 2014-06-04 14:59:01Z bastiaan $

module Ideas.Common.Strategy.Choice 
   ( Choice(..), Menu
   , isStop, elems, bests
   , bind, mapWithIndex, getByIndex
   ) where
   
import Data.Maybe (listToMaybe)

class Choice f where
   single :: a -> f a
   stop   :: f a
   (<|>)  :: f a -> f a -> f a
   (|>)   :: f a -> f a -> f a
   oneof  :: [a]   -> f a
   choice :: [f a] -> f a
   -- default implementation
   oneof = choice . map single
   choice xs 
      | null xs   = stop
      | otherwise = foldr1 (<|>) xs

instance Choice [] where
   single   = return
   stop     = []
   (<|>)    = (++)
   xs |> ys = if null xs then ys else xs
   oneof    = id
   choice   = concat

instance Choice Menu where
   single = Single
   stop   = Stop
   
   p0 <|> rest = rec p0
    where
     rec Stop      = rest
     rec (p :|: q) = p :|: rec q
     rec p         = p :|: rest
     
   p0 |> rest = rec p0
    where
     rec Stop      = rest
     rec (p :|> q) = p :|> rec q
     rec p         = p :|> rest

data Menu a = Menu a :|: Menu a
            | Menu a :|> Menu a
            | Single a
            | Stop

instance Functor Menu where
   fmap f p = p >>= (Single . f)

instance Monad Menu where
   return = single
   fail _ = stop
   (>>=)  = bind

isStop :: Menu a -> Bool
isStop = null . elems

bests :: Menu a -> [a]
bests (p :|: q)  = bests p <|> bests q
bests (p :|> q)  = bests p  |> bests q
bests (Single a) = [a]
bests Stop       = []

elems :: Menu a -> [a]
elems = ($ []) . rec
 where
   rec (p :|: q)  = rec p . rec q
   rec (p :|> q)  = rec p . rec q
   rec (Single p) = (p:)
   rec Stop       = id

-- generalized monadic bind
{-# INLINE bind #-}
bind :: Choice f => Menu a -> (a -> f b) -> f b
bind m f = rec m
 where
   rec (p :|: q) =  rec p <|> rec q 
   rec (p :|> q)  = rec p  |> rec q
   rec (Single a) = f a
   rec Stop       = stop

{-# INLINE mapWithIndex #-}
mapWithIndex :: Choice f => (Int -> a -> b) -> Menu a -> f b
mapWithIndex f = snd . rec 0
 where
   rec n (p :|: q)  = let (n1, pn) = rec n p 
                          (n2, qn) = rec n1 q
                      in (n2, pn <|> qn)
   rec n (p :|> q)  = let (n1, pn) = rec n p 
                          (n2, qn) = rec n1 q
                      in (n2, pn |> qn)
   rec n (Single a) = (n+1, single (f n a))
   rec n Stop       = (n, stop)
                  
getByIndex :: Int -> Menu a -> Maybe a
getByIndex n = listToMaybe . drop n . elems