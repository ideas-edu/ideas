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
   , ErrorJSON, errorStr
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
import Data.List
import Data.String

type GDecoderJSON env = Decoder env ErrorJSON (Loc, JSONBuilder)

evalGDecoderJSON :: GDecoderJSON env a -> env -> JSON -> Either ErrorJSON a
evalGDecoderJSON p env json = evalDecoder p env (Root 0, jsonToBuilder json)

type DecoderJSON = GDecoderJSON ()

evalDecoderJSON :: DecoderJSON a -> JSON -> Either ErrorJSON a
evalDecoderJSON p = evalGDecoderJSON p ()

jObject :: GDecoderJSON env a -> GDecoderJSON env a
jObject p = jFirst $ \loc a -> 
   case a of 
      Just (Object xs) -> put (loc, mconcat $ map (uncurry tagJSON) xs) >> p
      _ -> errorJSON NotAnObject loc a

jKey :: Key -> GDecoderJSON env a -> GDecoderJSON env a
jKey k p = get >>= \(loc, xs) -> 
   case extractKey k xs of
      Just (v, rest) -> put (LocKey 0 k loc, jsonToBuilder v) *> p <* put (loc, rest)
      _ -> errorJSON (KeyNotFound k) loc (Just (builderToJSON xs))

jWithKeys :: (Key -> GDecoderJSON env a) -> GDecoderJSON env [a]
jWithKeys f = get >>= \(loc, xs) -> 
   mapM (\(k, v) -> put (LocKey 0 k loc, jsonToBuilder v) >> f k) (extractKeyAndValues xs)

jObjectWithKeys :: (Key -> GDecoderJSON env a) -> GDecoderJSON env [a]
jObjectWithKeys = jObject . jWithKeys

jArray :: GDecoderJSON env a -> GDecoderJSON env a
jArray p = jFirst $ \loc a -> 
   case a of 
      Just (Array xs) -> put (LocArray 0 loc, mconcat $ map jsonToBuilder xs) >> p
      _ -> errorJSON NotAnArray loc a

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
jString = jFirst $ \loc a ->
   case a of
      Just (String s) -> return s
      _ -> errorJSON NotAString loc a

jChar :: GDecoderJSON env Char
jChar = jFirst $ \loc a ->
   case a of
      Just (String [c]) -> return c
      _ -> errorJSON NotAChar loc a

jBool :: GDecoderJSON env Bool
jBool = jFirst $ \loc a ->
   case a of
      Just (Boolean b) -> return b
      _ -> errorJSON NotABoolean loc a

jInteger :: GDecoderJSON env Integer
jInteger = jFirst $ \loc a ->
   case a of
      Just (Number (I i)) -> return i
      _ -> errorJSON NotAnInteger loc a

jInt :: GDecoderJSON env Int
jInt = fromInteger <$> jInteger

jDouble :: GDecoderJSON env Double
jDouble = jFirst $ \loc a ->
   case a of
      Just (Number (D d)) -> return d
      _ -> errorJSON NotADouble loc a

jFloat :: GDecoderJSON env Float
jFloat = realToFrac <$> jDouble

jNull :: GDecoderJSON env ()
jNull = jFirst $ \loc a ->
   case a of
      Just (Null) -> return ()
      _ -> errorJSON NotNull loc a

jEmpty :: GDecoderJSON env ()
jEmpty = get >>= \(loc, xs) ->
   unless (isEmptyBuilder xs) (errorJSON NotEmpty loc (Just (builderToJSON xs)))

jSkip :: GDecoderJSON env ()
jSkip = jNext $ const $ return ()

jNext :: (JSON -> Either String a) -> GDecoderJSON env a
jNext f = jFirst $ \loc mjson -> 
   case fmap f mjson of
      Just res -> either errorStr return res
      Nothing  -> errorJSON NoNextElement loc Nothing

-- local helper: not exported
jFirst :: (Loc -> Maybe JSON -> GDecoderJSON env a) -> GDecoderJSON env a
jFirst f = get >>= \(loc, xs) ->
   case extractFirst xs of
      Just (json, rest) -> f loc (Just json) <* put (nextLoc loc, rest)
      _ -> f loc Nothing

--------------------------------------------------------------------------------
-- Errors

newtype ErrorJSON = E [Either String (ErrorType, Loc, Maybe JSON)]

instance Show ErrorJSON where
   show (E xs)
      | null xs   = "Parse error in JSON"
      | otherwise = unlines (map (either id f) xs)
    where
      
      f (tp, loc, mjson) = unlines
         [ "Parse error: " ++ show tp
         , "  * Location: " ++ show loc
         , "  * Found: " ++ maybe "" show mjson
         ]

data ErrorType 
   = NotAnObject | NotAnArray | NotAString | NotAChar | NotABoolean | NotAnInteger | NotADouble
   | NotNull | NotEmpty | KeyNotFound Key | NoNextElement

instance Show ErrorType where
   show NotAnObject     = "not an object"
   show NotAnArray      = "not an array"
   show NotAString      = "not a string"
   show NotAChar        = "not a char"
   show NotABoolean     = "not a boolean"
   show NotAnInteger    = "not an integer"
   show NotADouble      = "not a double"
   show NotNull         = "not null"
   show NotEmpty        = "not empty"
   show (KeyNotFound k) = "key '" ++ k ++ "' not found"
   show NoNextElement   = "no next element"

instance IsString ErrorJSON where
   fromString s = E [Left s]

instance Semigroup ErrorJSON where
   E xs <> E ys = E (xs <> ys)

instance Monoid ErrorJSON where
   mempty = E []

data Loc = Root Int | LocArray Int Loc | LocKey Int Key Loc

instance Show Loc where
   show loc
      | null parts = "root"
      | otherwise  = intercalate "." parts
    where
      parts = collect loc

      collect (Root n)       = [ "root+" ++ show n | n > 0 ]
      collect (LocArray n l) = collect l ++ [show n]
      collect (LocKey n k l) = collect l ++ [k ++ if n==0 then "" else "+" ++ show n]

nextLoc :: Loc -> Loc
nextLoc (Root n)       = Root (n+1)
nextLoc (LocArray n l) = LocArray (n+1) l
nextLoc (LocKey n k l) = LocKey (n+1) k l

errorStr :: String -> GDecoderJSON env a
errorStr = throwError . fromString

errorJSON :: ErrorType -> Loc -> Maybe JSON -> GDecoderJSON env a
errorJSON tp loc a = throwError $ E [Right (tp, loc, a)]