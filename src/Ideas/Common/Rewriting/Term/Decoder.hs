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
   , tCon, tCon0, tCon1, tCon2, tConOf, tInteger, tDouble, tVar, tListOf, tConWithSymbol
   , tList2, tList3, tChar, tFirst
     -- re-exports
   , Alternative(..), MonadReader(..), throwError
   ) where

import Control.Monad
import Ideas.Common.Rewriting.Term.Data
import Ideas.Utils.Decoding

-----------------------------------------------------------
-- * Type class for conversion to/from terms

type TermDecoder = Decoder () String [Term]

tFirst :: (Maybe Term -> TermDecoder a) -> TermDecoder a
tFirst f = get >>= \xs ->
   case xs of
      t:rest -> f (Just t) <* put rest
      _ -> f Nothing

tVar :: TermDecoder String
tVar = tFirst $ \mt ->
   case mt of 
      Just (TVar s) -> return s
      _ -> throwError "not a var"

tChar :: TermDecoder Char
tChar = tFirst $ \mt ->
   case mt of 
      Just (TVar [c]) -> return c
      _ -> throwError "not a char"

tInteger :: TermDecoder Integer
tInteger = tFirst $ \mt ->
   case mt of 
      Just (TNum i) -> return i
      _ -> throwError "not an integer"

tDouble :: TermDecoder Double
tDouble = tFirst $ \mt ->
   case mt of 
      Just (TFloat f) -> return f
      _ -> throwError "not a double"

tList :: TermDecoder a -> TermDecoder a
tList p = tFirst $ \mt ->
   case mt of 
      Just (TList xs) -> put xs *> p <* tEmpty
      _ -> throwError "not a list"

tCon :: Symbol -> TermDecoder a -> TermDecoder a
tCon s1 p = tFirst $ \mt ->
   case mt of 
      Just (TCon s2 xs) | s1 == s2 -> put xs *> p <* tEmpty
      _ -> throwError $ "not con " ++ show s1

tEmpty :: TermDecoder ()
tEmpty = get >>= \xs -> unless (null xs) (throwError "not empty") 

tCon0 :: Symbol -> TermDecoder ()
tCon0 s = tCon s (return ())

tCon1 :: Symbol -> (a -> b) -> TermDecoder a -> TermDecoder b
tCon1 s f p = tCon s (f <$> p)

tCon2 :: Symbol -> (a -> b -> c) -> TermDecoder a -> TermDecoder b -> TermDecoder c
tCon2 s f p q = tCon s (f <$> p <*> q)

tConOf :: Symbol -> TermDecoder a -> TermDecoder [a]
tConOf s p = tCon s (many p)

tConWithSymbol :: (Symbol -> [a] -> b) -> TermDecoder a -> TermDecoder b
tConWithSymbol f p = tFirst $ \mt ->
   case mt of 
      Just (TCon s xs) -> f s <$ put xs <*> many p <* tEmpty
      _ -> throwError $ "not a con"

tList2 :: (a -> b -> c) -> TermDecoder a -> TermDecoder b -> TermDecoder c
tList2 f p q = tList $ f <$> p <*> q

tList3 :: (a -> b -> c -> d) -> TermDecoder a -> TermDecoder b -> TermDecoder c -> TermDecoder d
tList3 f p q r = tList $ f <$> p <*> q <*> r

tListOf :: TermDecoder a -> TermDecoder [a]
tListOf p = tList $ many p