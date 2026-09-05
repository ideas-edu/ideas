-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Text.JSON.Builder
   ( JSONBuilder, arrayBuilder, jsonToBuilder, tagJSON, builderToJSON
   , isEmptyBuilder
   , extractFirst, extractKey, extractKeyAndValues
   ) where

import Data.Maybe
import Ideas.Text.JSON.Data

--------------------------------------------------------
-- JSON builder

newtype JSONBuilder = JB [(Maybe Key, JSON)]

instance Semigroup JSONBuilder where
   JB xs <> JB ys = JB (xs <> ys)

instance Monoid JSONBuilder where
   mempty = JB []

jsonToBuilder :: JSON -> JSONBuilder
jsonToBuilder a = JB [(Nothing, a)]

tagJSON :: String -> JSON -> JSONBuilder
tagJSON s a = JB [(Just s, a)]

arrayBuilder :: [JSONBuilder] -> JSONBuilder
arrayBuilder = jsonToBuilder . Array . map builderToJSON

isEmptyBuilder :: JSONBuilder -> Bool
isEmptyBuilder (JB xs) = null xs

extractFirst :: JSONBuilder -> Maybe (JSON, JSONBuilder)
extractFirst (JB ((Nothing, a):rest)) = Just (a, JB rest)
extractFirst _ = Nothing

extractKey :: Key -> JSONBuilder -> Maybe (JSON, JSONBuilder)
extractKey k (JB xs) =
   case break (maybe False (== k) . fst) xs of
      (xs1, (_, v):xs2) -> Just (v, JB (xs1 ++ xs2))
      _ -> Nothing

extractKeyAndValues :: JSONBuilder -> [(Key, JSON)]
extractKeyAndValues (JB xs) =  [ (k, a) | (mk, a) <- xs, k <- maybeToList mk ]

builderToJSON :: JSONBuilder -> JSON
builderToJSON (JB [(Nothing, x)]) = x
builderToJSON (JB xs) = 
   case mapM fst xs of
      Just ks -> Object (zip ks (map snd xs))
      Nothing -> Array (map f xs)
 where
    f (Nothing, a) = a
    f (Just k, a)  = Object [(k, a)]