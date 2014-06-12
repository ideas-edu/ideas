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
--  $Id$

module Ideas.Common.Strategy.Sequential
   ( Sequential(..), Choice(..)
   , Process
   , Builder, build
   , empty, firsts, scanChoice, prune, accum
   
   , fromAtoms, Sym(..), atomic, concurrent, (<@>)
   
   , withPath, replay
   , uniquePath
   ) where

import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Path
 
-----------------------------------------------------

class Choice f => Sequential f where
   ok    :: f a
   (~>)  :: a -> f a -> f a
   (<*>) :: f a -> f a -> f a
   -- default implementation
   a ~> p = single a <*> p

infixr 3 :~>, ~>

newtype Process a = P { getMenu :: Menu (Step a) }

data Step a = a :~> Process a
            | Ok

onStep :: Sequential f => (a -> Process a -> f b) -> Step a -> f b
onStep f (a :~> p) = f a p
onStep _ Ok        = ok

{-# INLINE changeStep #-}
changeStep :: Sequential f => (a -> Process a -> f b) -> Process a -> f b
changeStep f p = bind (getMenu p) (onStep f)

isOk :: Step a -> Bool
isOk Ok = True
isOk _  = False

instance Choice Process where
   single a = P (single (a :~> ok))
   stop  = P stop
   P xs <|> P ys = P (xs <|> ys)
   P xs  |> P ys = P (xs  |> ys)
  
instance Sequential Process where
   ok     = P (single Ok)
   a ~> p = P (single (a :~> p))
   
   P xs <*> q = xs `bind` f
    where
      f (a :~> p) = a ~> (p <*> q)
      f Ok = q
    
newtype Builder a = B (Process a -> Process a)

instance Choice Builder where
   single a = B (a ~>)
   stop        = B (const stop)
   B f <|> B g = B (\p -> f p <|> g p)
   B f  |> B g = B (\p -> f p  |> g p)
   
instance Sequential Builder where
   ok          = B id
   a ~> B f    = B ((a ~>) . f)
   B f <*> B g = B (f . g)

build :: Builder a -> Process a
build (B f) = f ok

empty :: Process a -> Bool
empty = any isOk . bests . getMenu

firsts :: Process a -> [(a, Process a)]
firsts p = [ (a, q) | a :~> q <- bests (getMenu p) ]

{-# INLINE scanChoice #-}
scanChoice :: (a -> b -> [(a, c)]) -> a -> Process b -> Process c
scanChoice f = rec
 where
   rec a = changeStep $ \b p ->
      choice [ (c ~> rec a2 p) | (a2, c) <- f a b ]

{-# INLINE accum #-}
accum :: (b -> a -> [a]) -> a -> Process b -> Menu a
accum f = rec 
 where
   rec a p = getMenu p >>= g
    where
      g Ok = return a
      g (b :~> q) = choice [ rec c q  | c <- f b a ]

 --where
--   rec a = changeStep $ \b p ->
      --choice [ (rec a2 p) | (a2, c) <- f a b ]

-- fail early
prune :: (a -> Bool) -> Process a -> Process a
prune f = rec 
 where
   rec = changeStep $ \a p -> 
      case P (cut (getMenu (rec p))) of
         np | not (f a) && isStop (getMenu np) -> stop
            | otherwise -> a ~> np

useFirst :: Sequential f => (a -> Process a -> f b) -> f b -> Process a -> f b
useFirst op e p = bind (getMenu p) f
 where
   f Ok = e
   f (a :~> q) = op a q

data Sym a = Single a | Composed (Process a)

fromAtoms :: Process (Sym a) -> Process a
fromAtoms = changeStep $ \sym q -> 
   case sym of
      Single a   -> a ~> fromAtoms q
      Composed p -> p <*> fromAtoms q

atomic :: Sequential f => Process (Sym a) -> f (Sym a)
atomic = single . Composed . fromAtoms

concurrent :: Sequential f => (a -> Bool) -> Process a -> Process a -> f a
concurrent switch = normal
 where
   normal p q = stepBoth q p <|> (stepRight q p <|> stepRight p q)

   stepBoth  = useFirst stop2 . useFirst stop2 ok
   stop2 _ _ = stop

   stepRight p = useFirst op stop
    where
      op a = (a ~>) . (if switch a then normal else stepRight) p

-- Alternate combinator
(<@>) :: Sequential f => Process a -> Process a -> f a
p <@> q = useFirst (\a r -> a ~> (q <@> r)) bothOk p
 where
   bothOk = useFirst (\_ _ -> stop) ok q

withPath :: Process a -> Process (a, Path)
withPath = rec []
 where
   rec ns = mapWithIndex (onStep . f) . getMenu
    where 
      f n a p = (a, fromIntList (reverse ms)) ~> rec ms p
       where
         ms = n:ns

replay :: Monad m => Path -> Process a -> m ([a], Process a)
replay = rec [] . intList
 where
   rec acc [] p = return (acc, p)
   rec acc (n:ns) p =
      case getByIndex n (getMenu p) of
         Just (a :~> q) -> rec (a:acc) ns q
         _ ->  fail "replay: invalid path"

---------------------------------------------------------------------------
   
tidyProcess :: (a -> a -> Bool) -> (a -> Bool) -> Process a -> Process a
tidyProcess eq cond = rec 
 where
   rec = f . elems . getMenu
 
   f xs | any isOk xs = ok
        | otherwise   = choice (rmSameChoice (make xs))
        
   make xs = [ rmPrefix a (rec p) | a :~> p <- xs ]
        
   rmPrefix a p | cond a    = p
                | otherwise = a ~> p
                
   -- unnecessary?
   rmSameChoice (p:q:rs) 
      | cmpProcesses eq p q = rmSameChoice (p:rs)
      | otherwise           = p:rmSameChoice (q:rs)
   rmSameChoice xs = xs

-- | The uniquePath transformation changes the process in such a way that all
--   intermediate states can only be reached by one path. A prerequisite is that
--   symbols are unique (or only used once).
uniquePath :: (a -> Bool) -> (a -> a -> Bool) -> Process a -> Process a
uniquePath cond eq = tidyProcess eq (not . cond) . rec
 where
   rec = process . elems . getMenu
 
   process [] = stop
   process (Ok:xs) = ok <|> process xs
   process ((a :~> p):xs) = 
      let ys = map fst $ firstsWith cond (a ~> p)
      in (a ~> rec p) <|> process (concatMap (change ys) xs)

   change _  Ok        = [Ok]
   change ys (a :~> q) = 
      let f x = all (not . eq x) ys
      in elems $ getMenu $ filterP f (a ~> q)

cmpProcesses:: (a -> b -> Bool) -> Process a -> Process b -> Bool
cmpProcesses eq p q = 
   length as == length bs && all cmp (zip as bs)
 where
   as = elems (getMenu p)
   bs = elems (getMenu q)

   cmp (a :~> pa, b :~> qa) = eq a b && cmpProcesses eq pa qa
   cmp (Ok, Ok) = True
   cmp _ = False

filterP :: (a -> Bool) -> Process a -> Process a
filterP cond = rec 
 where
   rec = changeStep $ \a q -> 
      if cond a then a ~> rec q else stop

firstsWith :: (a -> Bool) -> Process a -> [(a, Process a)]
firstsWith cond = rec
 where
   rec = concatMap f . firsts
   f (a, p) = if cond a then [(a, p)] else rec p