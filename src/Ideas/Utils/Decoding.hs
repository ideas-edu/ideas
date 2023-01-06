{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
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

module Ideas.Utils.Decoding
   ( Decoder, evalDecoder, runDecoder, mapError, getLoc, putLoc, changeLoc
   , Encoder, runEncoder
   , Error, ErrorType, Loc(..), nextLoc, raiseError, errorStr
     -- re-exports
   , Alternative(..), optional, MonadReader(..), MonadState(..), MonadError(..)
   , gets
   ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.String

-------------------------------------------------------------------

newtype Decoder env err s a = Dec { fromDec :: StateT (Loc, s) (ReaderT env (Except err)) a }
 deriving (Functor, Applicative, Alternative, MonadPlus, MonadReader env, MonadError err)

instance Semigroup a => Semigroup (Decoder env err s a) where
   (<>) = liftA2 (<>)

instance Monoid a => Monoid (Decoder env err s a) where
   mempty  = pure mempty

instance Monad (Decoder env err s) where
   Dec m >>= f = Dec $ m >>= fromDec . f

instance MonadState s (Decoder env err s) where
   state f = Dec $ state $ \(loc, s) -> let (a, s') = f s in (a, (loc, s'))

runDecoder :: Decoder env err s a -> env -> s -> Either err (a, s)
runDecoder p env s = fmap snd <$> runExcept (runReaderT (runStateT (fromDec p) (Root 0, s)) env)

evalDecoder :: Decoder env err s a -> env -> s -> Either err a
evalDecoder p env = fmap fst . runDecoder p env

mapError ::  (err1 -> err2) -> Decoder env err1 s a -> Decoder env err2 s a
mapError f p = do
   env <- reader id
   s1  <- get
   case runDecoder p env s1 of
      Left e1 -> throwError (f e1)
      Right (a, s2) -> put s2 >> return a

getLoc :: Decoder env err s Loc
getLoc = Dec $ gets fst

putLoc :: Loc -> Decoder env err s ()
putLoc = changeLoc . const 

changeLoc :: (Loc -> Loc) -> Decoder env err s ()
changeLoc = Dec . modify . first

-------------------------------------------------------------------

type Encoder env err = Decoder env err ()

runEncoder :: Encoder env err a -> env -> Either err a
runEncoder p env = evalDecoder p env ()

--------------------------------------------------------------------------------
-- Errors

newtype Error a = E [Either String (ErrorType, Loc, Maybe a)]

type ErrorType = String 

instance Show a => Show (Error a) where
   show (E xs)
      | null xs   = "Parse error"
      | otherwise = unlines (map (either id f) xs)
    where
      
      f (tp, loc, ma) = unlines
         [ "Parse error: " ++ tp
         , "  * Location: " ++ show loc
         , "  * Found: " ++ maybe "" show ma
         ]

instance IsString (Error a) where
   fromString s = E [Left s]

instance Semigroup (Error a) where
   E xs <> E ys = E (xs <> ys)

instance Monoid (Error a) where
   mempty = E []

data Loc = Root Int | LocByPos Int Loc | LocByKey Int String Loc

instance Show Loc where
   show loc
      | null parts = "root"
      | otherwise  = intercalate "." parts
    where
      parts = collect loc

      collect (Root n)         = [ "root+" ++ show n | n > 0 ]
      collect (LocByPos n l)   = collect l ++ [show n]
      collect (LocByKey n k l) = collect l ++ [k ++ if n==0 then "" else "+" ++ show n]

nextLoc :: Loc -> Loc
nextLoc (Root n)         = Root (n+1)
nextLoc (LocByPos n l)   = LocByPos (n+1) l
nextLoc (LocByKey n k l) = LocByKey (n+1) k l

errorStr :: IsString err => String -> Decoder env err s a
errorStr = throwError . fromString

raiseError :: ErrorType -> Maybe a -> Decoder env (Error a) s b
raiseError tp a = do
   loc <- getLoc
   throwError $ E [Right (tp, loc, a)]