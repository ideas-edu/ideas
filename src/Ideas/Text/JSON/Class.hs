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

module Ideas.Text.JSON.Class ( InJSON(..), (.=) ) where

import Control.Monad.State
import Ideas.Text.JSON.Builder
import Ideas.Text.JSON.Data
import Ideas.Text.JSON.Decoder

class InJSON a where
   toJSON          :: a -> JSON
   listToJSON      :: [a] -> JSON
   fromJSON        :: JSON -> Maybe a
   jsonBuilder     :: a -> JSONBuilder
   jsonListBuilder :: [a] -> JSONBuilder
   jsonDecoder     :: DecoderJSON a
   jsonListDecoder :: DecoderJSON [a]

   {-# MINIMAL (toJSON | jsonBuilder), jsonDecoder #-}

   -- default definitions
   toJSON     = builderToJSON . jsonBuilder
   listToJSON = Array . map toJSON
   fromJSON   = either (fail . show) return . evalDecoderJSON jsonDecoder

   jsonBuilder     = jsonToBuilder . toJSON
   jsonListBuilder = mconcat . map jsonBuilder  
   jsonListDecoder = jArrayOf jsonDecoder

instance InJSON Int where
   toJSON      = toJSON . toInteger
   jsonDecoder = jInt

instance InJSON Integer where
   toJSON      = Integer
   jsonDecoder = jInteger

instance InJSON Double where
   toJSON = Double
   jsonDecoder = jDouble

instance InJSON Char where
   toJSON c        = String [c]
   listToJSON      = String
   jsonListBuilder = jsonToBuilder . String 
   jsonDecoder     = jChar
   jsonListDecoder = jString

instance InJSON Bool where
   toJSON = Boolean
   jsonDecoder = jBool

instance InJSON a => InJSON [a] where
   toJSON      = listToJSON
   jsonBuilder = jsonListBuilder
   jsonDecoder = jsonListDecoder

instance InJSON () where
   jsonBuilder = mempty
   jsonDecoder = return ()

instance (InJSON a, InJSON b) => InJSON (a, b) where
   jsonBuilder (a, b) = jsonBuilder a <> jsonBuilder b
   jsonDecoder = (,) <$> jsonDecoder <*> jsonDecoder

instance (InJSON a, InJSON b, InJSON c) => InJSON (a, b, c) where
   jsonBuilder (a, b, c) = jsonBuilder a <> jsonBuilder b <> jsonBuilder c
   jsonDecoder = (,,) <$> jsonDecoder <*> jsonDecoder <*> jsonDecoder

instance (InJSON a, InJSON b, InJSON c, InJSON d) => InJSON (a, b, c, d) where
   jsonBuilder (a, b, c, d) = jsonBuilder a <> jsonBuilder b <> jsonBuilder c <> jsonBuilder d
   jsonDecoder = (,,,) <$> jsonDecoder <*> jsonDecoder <*> jsonDecoder <*> jsonDecoder

instance InJSON JSON where
   toJSON      = id
   jsonDecoder = gets builderToJSON

instance InJSON JSONBuilder where
   jsonBuilder = id
   jsonDecoder = get

infix 7 .=

(.=) :: InJSON a => String -> a -> JSONBuilder
s .= a = tagJSON s (toJSON a)