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

module Ideas.Text.JSON.Decoder
   ( GDecoderJSON, evalGDecoderJSON
   , DecoderJSON, evalDecoderJSON
   , Error, errorStr
   , jObject, jKey, jWithKeys, jObjectWithKeys
   , jArray, jArrayOf, jArray0, jArray1, jArray2, jArray3
   , jString, jChar, jBool, jInteger, jInt, jDouble, jFloat
   , jNull, jEmpty, jSkip
   , jNext
     -- re-exports
   , Alternative(..), MonadReader(..), throwError
   ) where

import Control.Monad
import Ideas.Text.JSON.Builder
import Ideas.Text.JSON.Data
import Ideas.Utils.Decoding

type GDecoderJSON env = Decoder env (Error JSON) JSONBuilder

evalGDecoderJSON :: GDecoderJSON env a -> env -> JSON -> Either (Error JSON) a
evalGDecoderJSON p env = evalDecoder p env . jsonToBuilder

type DecoderJSON = GDecoderJSON ()

evalDecoderJSON :: DecoderJSON a -> JSON -> Either (Error JSON) a
evalDecoderJSON p = evalGDecoderJSON p ()

jObject :: GDecoderJSON env a -> GDecoderJSON env a
jObject p = jFirst $ \a -> 
   case a of 
      Just (Object xs) -> put (mconcat $ map (uncurry tagJSON) xs) >> p
      _ -> raiseError "not an object" a

jKey :: Key -> GDecoderJSON env a -> GDecoderJSON env a
jKey k p = get2 >>= \(loc, xs) -> 
   case extractKey k xs of
      Just (v, rest) -> put2 (LocByKey 0 k loc, jsonToBuilder v) *> p <* put2 (loc, rest)
      _ -> raiseError ("key '" ++ k ++ "' not found") (Just (builderToJSON xs))

jWithKeys :: (Key -> GDecoderJSON env a) -> GDecoderJSON env [a]
jWithKeys f = get2 >>= \(loc, xs) ->
   mapM (\(k, v) -> put2 (LocByKey 0 k loc, jsonToBuilder v) >> f k) (extractKeyAndValues xs)

jObjectWithKeys :: (Key -> GDecoderJSON env a) -> GDecoderJSON env [a]
jObjectWithKeys = jObject . jWithKeys

jArray :: GDecoderJSON env a -> GDecoderJSON env a
jArray p = jFirst $ \a -> 
   case a of 
      Just (Array xs) -> changeLoc (LocByPos 0) >> put (mconcat $ map jsonToBuilder xs) >> p
      _ -> raiseError "not an array" a

jArrayOf :: GDecoderJSON env a -> GDecoderJSON env [a]
jArrayOf p = jArray $ many p <* jEmpty

jArray0 :: GDecoderJSON env ()
jArray0 = jArray jEmpty

jArray1 :: GDecoderJSON env a -> GDecoderJSON env a
jArray1 p = jArray $ p <* jEmpty

jArray2 :: (a -> b -> c) -> GDecoderJSON env a -> GDecoderJSON env b -> GDecoderJSON env c
jArray2 f p q = jArray $ f <$> p <*> q <* jEmpty

jArray3 :: (a -> b -> c -> d) -> GDecoderJSON env a -> GDecoderJSON env b -> GDecoderJSON env c -> GDecoderJSON env d
jArray3 f p q r = jArray $ f <$> p <*> q <*> r <* jEmpty

jString :: GDecoderJSON env String
jString = jFirst $ \a ->
   case a of
      Just (String s) -> return s
      _ -> raiseError "not a string" a

jChar :: GDecoderJSON env Char
jChar = jFirst $ \a ->
   case a of
      Just (String [c]) -> return c
      _ -> raiseError "not a char" a

jBool :: GDecoderJSON env Bool
jBool = jFirst $ \a ->
   case a of
      Just (Boolean b) -> return b
      _ -> raiseError "not a boolean" a

jInteger :: GDecoderJSON env Integer
jInteger = jFirst $ \a ->
   case a of
      Just (Integer i) -> return i
      _ -> raiseError "not an integer" a

jInt :: GDecoderJSON env Int
jInt = fromInteger <$> jInteger

jDouble :: GDecoderJSON env Double
jDouble = jFirst $ \a ->
   case a of
      Just (Double d) -> return d
      _ -> raiseError "not a double" a

jFloat :: GDecoderJSON env Float
jFloat = realToFrac <$> jDouble

jNull :: GDecoderJSON env ()
jNull = jFirst $ \a ->
   case a of
      Just Null -> return ()
      _ -> raiseError "not null" a

jEmpty :: GDecoderJSON env ()
jEmpty = get >>= \xs ->
   unless (isEmptyBuilder xs) (raiseError "not empty" (Just (builderToJSON xs)))

jSkip :: GDecoderJSON env ()
jSkip = jNext $ const $ return ()

jNext :: (JSON -> Either String a) -> GDecoderJSON env a
jNext f = jFirst $ \mjson -> 
   case fmap f mjson of
      Just res -> either errorStr return res
      Nothing  -> raiseError "no next element" Nothing

-- local helper: not exported
jFirst :: (Maybe JSON -> GDecoderJSON env a) -> GDecoderJSON env a
jFirst f = get >>= \xs ->
   case extractFirst xs of
      Just (json, rest) -> f (Just json) <* changeLoc nextLoc <* put rest
      _ -> f Nothing

get2 :: GDecoderJSON env (Loc, JSONBuilder)
get2 = (,) <$> getLoc <*> get

put2 :: (Loc, JSONBuilder) -> GDecoderJSON env ()
put2 (loc, xs) = putLoc loc >> put xs