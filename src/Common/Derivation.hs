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
-- Datatype for a tree of derivations. The datatype stores all intermediate
-- results as well as the rules used.
--
-----------------------------------------------------------------------------
module Common.Derivation 
   ( restrictHeight, restrictMajor, stepsMax, makeDerivation
   ) where

import Common.Utils (safeHead)
import Common.Apply
import Common.Strategy (IsStrategy(..), LabeledStrategy, Strategy, noLabels, unlabel)
import Common.Transformation
import qualified Common.Grammar as RE
import Data.Maybe

data DerivationTree a = DT a Bool [(Rule a, DerivationTree a)] 
   deriving Show

type Derivations a = [Derivation a]

data Derivation a = D a [(Rule a, a)]
   deriving Show

showDerivation :: Show a => Derivation a -> String
showDerivation = showDerivationWith show

showDerivationWith :: (a -> String) -> Derivation a -> String
showDerivationWith f (D a xs) = unlines $
   f a : concatMap (\(r, b) -> ["   => " ++ show r, f b]) xs

toDerivations :: DerivationTree a -> Derivations a
toDerivations (DT a b xs) = map (D a) $
   [ [] | b ] ++
   [ ((r,a2):ys) | (r, t) <- xs, D a2 ys <- toDerivations t ]

alternatives :: DerivationTree a -> [(Rule a, DerivationTree a)] 
alternatives (DT _ _ xs) = xs

ready :: DerivationTree a -> Bool
ready (DT _ b _) = b

makeDerivation :: IsStrategy f => f a -> a -> DerivationTree a
makeDerivation = grammarDerivation . noLabels . toStrategy

grammarDerivation :: RE.Grammar (Rule a) -> a -> DerivationTree a
grammarDerivation s a = DT a (RE.empty s) list
 where
   list = [ (f, grammarDerivation rest b) 
          | (f, rest) <- RE.firsts s
          , b <- applyAll f a 
          ]

{-
majorDerivation :: RE.Grammar (Rule a) -> a -> Derivation Rule a
majorDerivation s a = Derivation a (RE.empty s || or bs) (concat xss)
 where
   (bs, xss) = unzip $ map f $ RE.firsts s
   f (rule, rest)
      | isMajorRule rule = (False, zip (Prelude.repeat rule) (map (majorDerivation rest) new))
      | otherwise = let (bs, xss) = unzip $ map (g . majorDerivation rest) new
                    in (or bs, concat xss)
    where
      new = applyAll rule a
      g (Derivation _ b xs) = (b, xs) -}

steps :: DerivationTree a -> Maybe Int
steps (DT _ b xs) 
   | b         = Just 0
   | otherwise = fmap succ $ safeHead $ catMaybes $ map (steps . snd) xs

stepsMax :: Int -> DerivationTree a -> Int
stepsMax n = fromMaybe n . rec 0
 where
   rec i (DT _ b xs) 
      | b || i==n = Just i
      | null xs   = Nothing
     --  | otherwise = safeHead $ catMaybes $ map (rec (i+1) . snd) xs
      | otherwise = rec (i+1) (snd $ head xs) -- early commit on a derivation path

restrictHeight :: Int -> DerivationTree a -> DerivationTree a
restrictHeight n (DT a b xs)
   | n == 0    = DT a b []
   | otherwise = DT a b (map f xs)
 where
   f (r, d) = (r, restrictHeight (n-1) d)

restrictMajor :: DerivationTree a -> DerivationTree a
restrictMajor = filterIntermediates isMajorRule

filterIntermediates :: (Rule a -> Bool) -> DerivationTree a -> DerivationTree a
filterIntermediates p = rec 
 where
   rec (DT a b xs) = 
      DT a (or (b:map (ready . snd) xs)) (concatMap f xs)
   f (r, d)
      | p r       = [(r, rec d)]
      | otherwise = alternatives (rec d)
