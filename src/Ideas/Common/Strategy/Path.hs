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
-- A path encodes a location (sub-part) of a strategy.
-- Use Show/Read type classes for serialization/deserialization
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Common.Strategy.Path
   ( Path, emptyPath
   , withPath, replayPath, uniquePath
   ) where

import Ideas.Common.Classes
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Sequence
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Derived

newtype Path = Path { ints :: [Int] }
   deriving Eq

instance Show Path where
   show = show . ints

instance Read Path where
   readsPrec _ = map (mapFirst Path) . readList

emptyPath :: Path
emptyPath = Path []

withPath :: Process a -> Process (a, Path)
withPath = rec []
 where
   rec ns = mapWithIndex (step done . f ns) . menu

   f ns n a p = 
      let ms = n:ns
      in (a, Path (reverse ms)) ~> rec ms p

replayPath :: Monad m => Path -> Process a -> m ([a], Process a)
replayPath = rec [] . ints
 where
   rec acc [] p = return (acc, p)
   rec acc (n:ns) p =
      case getByIndex n (menu p) of
         Just (a :~> q) -> rec (a:acc) ns q
         _ ->  fail "replay: invalid path"
         
-- | The uniquePath transformation changes the process in such a way that all
--   intermediate states can only be reached by one path. A prerequisite is that
--   symbols are unique (or only used once).
uniquePath :: (a -> Bool) -> (a -> a -> Bool) -> Process a -> Process a
uniquePath cond eq = tidy . rec
 where
   rec = process . elems . menu
 
   process [] = empty
   process (Done:xs) = done <|> process xs
   process ((a :~> p):xs) = 
      let ys = map fst $ firsts $ hide cond (a ~> p)
      in (a ~> rec p) <|> process (concatMap (change ys) xs)

   change _  Done      = [Done]
   change ys (a :~> q) = 
      let f x = all (not . eq x) ys
      in elems $ menu $ filterP f (a ~> q)
      
   tidy = make . elems . menu
    
   make xs 
      | any isDone xs = done
      | otherwise = rmSameChoice
           [ rmPrefix a (tidy p) | a :~> p <- xs ]
           
   rmPrefix a = if cond a then (a ~>) else id
                   
   -- unnecessary?
   rmSameChoice (p:q:rs) 
      | eqProcessBy eq p q = rmSameChoice (p:rs)
      | otherwise          = p <|> rmSameChoice (q:rs)
   rmSameChoice [x] = x
   rmSameChoice []  = empty