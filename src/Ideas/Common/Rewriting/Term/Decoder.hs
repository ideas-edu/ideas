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
-- Generic terms
--
-----------------------------------------------------------------------------

module Ideas.Common.Rewriting.Term.Decoder
   ( TermDecoder
   , tCon, tCon0, tCon1, tCon2, tCon3, tConOf, tInteger, tDouble, tVar, tListOf, tConWithSymbol
   , tListWith, tList2, tList3, tChar, tFirst
     -- re-exports
   , Alternative(..), MonadReader(..), throwError
   ) where

import Control.Monad
import Ideas.Common.Rewriting.Term.Data
import Ideas.Utils.Decoding

-----------------------------------------------------------
-- * Type class for conversion to/from terms

type TermDecoder = Decoder () (Error Term) [Term]

tFirst :: (Maybe Term -> TermDecoder a) -> TermDecoder a
tFirst f = get >>= \xs ->
   case xs of
      t:rest -> f (Just t) <* changeLoc nextLoc <* put rest
      _ -> f Nothing

tVar :: TermDecoder String
tVar = tFirst $ \mt ->
   case mt of 
      Just (TVar s) -> return s
      _ -> raiseError "not a var" mt

tChar :: TermDecoder Char
tChar = tFirst $ \mt ->
   case mt of 
      Just (TVar [c]) -> return c
      _ -> raiseError "not a char" mt

tInteger :: TermDecoder Integer
tInteger = tFirst $ \mt ->
   case mt of 
      Just (TNum i) -> return i
      _ -> raiseError "not an integer" mt

tDouble :: TermDecoder Double
tDouble = tFirst $ \mt ->
   case mt of 
      Just (TFloat f) -> return f
      _ -> raiseError "not a double" mt

-- name tList clashes with service type
tListWith :: TermDecoder a -> TermDecoder a
tListWith p = tFirst $ \mt ->
   case mt of 
      Just (TList xs) -> changeLoc (LocByPos 0) >> put xs *> p <* tEmpty
      _ -> raiseError "not a list" mt

tCon :: Symbol -> TermDecoder a -> TermDecoder a
tCon s1 p = tFirst $ \mt ->
   case mt of 
      Just (TCon s2 xs) | s1 == s2 -> changeLoc (LocByPos 0) >> put xs *> p <* tEmpty
      _ -> raiseError ("not con '" ++ show s1 ++ "'") mt

tEmpty :: TermDecoder ()
tEmpty = get >>= \xs -> unless (null xs) (errorStr "not empty") 

tCon0 :: Symbol -> TermDecoder ()
tCon0 s = tCon s (return ())

tCon1 :: Symbol -> (a -> b) -> TermDecoder a -> TermDecoder b
tCon1 s f p = tCon s (f <$> p)

tCon2 :: Symbol -> (a -> b -> c) -> TermDecoder a -> TermDecoder b -> TermDecoder c
tCon2 s f p q = tCon s (f <$> p <*> q)

tCon3 :: Symbol -> (a -> b -> c -> d) -> TermDecoder a -> TermDecoder b -> TermDecoder c -> TermDecoder d
tCon3 s f p q r = tCon s (f <$> p <*> q <*> r)

tConOf :: Symbol -> TermDecoder a -> TermDecoder [a]
tConOf s p = tCon s (many p)

tConWithSymbol :: (Symbol -> [a] -> b) -> TermDecoder a -> TermDecoder b
tConWithSymbol f p = tFirst $ \mt ->
   case mt of 
      Just (TCon s xs) -> f s <$ changeLoc (LocByPos 0) <* put xs <*> many p <* tEmpty
      _ -> raiseError "not a con" mt

tList2 :: (a -> b -> c) -> TermDecoder a -> TermDecoder b -> TermDecoder c
tList2 f p q = tListWith $ f <$> p <*> q

tList3 :: (a -> b -> c -> d) -> TermDecoder a -> TermDecoder b -> TermDecoder c -> TermDecoder d
tList3 f p q r = tListWith $ f <$> p <*> q <*> r

tListOf :: TermDecoder a -> TermDecoder [a]
tListOf p = tListWith $ many p