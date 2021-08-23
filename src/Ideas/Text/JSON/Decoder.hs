{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
   ( EnvDecoderJSON, runEnvDecoderJSON, fromEnvDecoderJSON
   , DecoderJSON, runDecoderJSON
   , ErrorJSON
   , jObject, jKey, jWithObject
   , jArray, jWithArray
   , jString, jBool, jInteger, jInt, jDouble, jFloat
   , jNull, jEmpty
     -- re-exports
   , Alternative(..), MonadReader(..)
   ) where

import Control.Monad
import Ideas.Text.JSON.Builder
import Ideas.Text.JSON.Data
import Ideas.Utils.Decoding
import Data.List
import Data.String

newtype EnvDecoderJSON env a = DJ { fromEnvDecoderJSON :: Decoder env ErrorJSON (Loc, JSONBuilder) a }
 deriving (Functor, Applicative, Alternative, Monad, MonadReader env)

runEnvDecoderJSON :: EnvDecoderJSON env a -> env -> JSON -> Either ErrorJSON a
runEnvDecoderJSON p env json = runDecoder (fromEnvDecoderJSON p) env (Root 0, builder json)

type DecoderJSON = EnvDecoderJSON ()

runDecoderJSON :: DecoderJSON a -> JSON -> Either ErrorJSON a
runDecoderJSON p = runEnvDecoderJSON p ()

jObject :: EnvDecoderJSON env a -> EnvDecoderJSON env a
jObject p = jNext $ \loc a -> 
   case a of 
      Just (Object xs) -> putDJ (loc, builderObject xs) >> p
      _ -> errorJSON NotAnObject loc a

jKey :: Key -> EnvDecoderJSON env a -> EnvDecoderJSON env a
jKey k p = getDJ >>= \(loc, xs) -> 
   case extractKey k xs of
      Just (v, rest) -> putDJ (LocKey 0 k loc, builder v) *> p <* putDJ (loc, rest)
      _ -> errorJSON (KeyNotFound k) loc (Just (toJSON xs))

jWithObject :: (Key -> EnvDecoderJSON env a) -> EnvDecoderJSON env [a]
jWithObject g = jObject $ getDJ >>= \(loc, xs) -> 
   mapM (\(k, v) -> putDJ (LocKey 0 k loc, builder v) >> g k) (extractKeyAndValues xs)

jArray :: EnvDecoderJSON env a -> EnvDecoderJSON env a
jArray p = jNext $ \loc a -> 
   case a of 
      Just (Array xs) -> putDJ (LocArray 0 loc, builderList xs) >> p
      _ -> errorJSON NotAnArray loc a

jWithArray :: EnvDecoderJSON env a -> EnvDecoderJSON env [a]
jWithArray p = jArray (many p <* jEmpty)

jString :: EnvDecoderJSON env String
jString = jNext $ \loc a ->
   case a of
      Just (String s) -> return s
      _ -> errorJSON NotAString loc a

jBool :: EnvDecoderJSON env Bool
jBool = jNext $ \loc a ->
   case a of
      Just (Boolean b) -> return b
      _ -> errorJSON NotABoolean loc a

jInteger :: EnvDecoderJSON env Integer
jInteger = jNext $ \loc a ->
   case a of
      Just (Number (I i)) -> return i
      _ -> errorJSON NotAnInteger loc a

jInt :: EnvDecoderJSON env Int
jInt = fromInteger <$> jInteger

jDouble :: EnvDecoderJSON env Double
jDouble = jNext $ \loc a ->
   case a of
      Just (Number (D d)) -> return d
      _ -> errorJSON NotADouble loc a

jFloat :: EnvDecoderJSON env Float
jFloat = realToFrac <$> jDouble

jNull :: EnvDecoderJSON env ()
jNull = jNext $ \loc a ->
   case a of
      Just (Null) -> return ()
      _ -> errorJSON NotNull loc a

jEmpty :: EnvDecoderJSON env ()
jEmpty = getDJ >>= \(loc, xs) -> 
   unless (isEmptyBuilder xs) (errorJSON NotEmpty loc (Just (toJSON xs)))

-- local helper: not exported
jNext :: (Loc -> Maybe JSON -> EnvDecoderJSON env a) -> EnvDecoderJSON env a
jNext f = getDJ >>= \(loc, xs) ->
   case extractFirst xs of
      Just (json, rest) -> f loc (Just json) <* putDJ (nextLoc loc, rest)
      _ -> f loc Nothing

-- local helper: not exported
getDJ :: EnvDecoderJSON env (Loc, JSONBuilder)
getDJ = DJ get

-- local helper: not exported
putDJ :: (Loc, JSONBuilder) -> EnvDecoderJSON env ()
putDJ = DJ . put

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
   = NotAnObject | NotAnArray | NotAString | NotABoolean | NotAnInteger | NotADouble
   | NotNull | NotEmpty | KeyNotFound Key

instance Show ErrorType where
   show NotAnObject     = "not an object"
   show NotAnArray      = "not an array"
   show NotAString      = "not a string"
   show NotABoolean     = "not a boolean"
   show NotAnInteger    = "not an integer"
   show NotADouble      = "not a double"
   show NotNull         = "not null"
   show NotEmpty        = "not empty"
   show (KeyNotFound k) = "key '" ++ k ++ "'not found"

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

errorJSON :: ErrorType -> Loc -> Maybe JSON -> EnvDecoderJSON env a
errorJSON tp loc a = DJ $ throwError $ E [Right (tp, loc, a)]