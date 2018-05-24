{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2018, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Encoding.Encoder
   ( -- * Converter type class
     Converter(..)
   , getExercise, getBaseUrl, getQCGen, getScript, getRequest
   , withExercise, withOpenMath, withJSONTerm, (//)
     -- * JSON support
   , hasJSONView, addJSONView, jsonEncoding
   , termToJSON, jsonToTerm
     -- * Latex support
   , hasLatexEncoding, latexPrinter, latexPrinterContext
   , latexEncoding, latexEncodingWith
     -- * Encoder datatype
   , Encoder, TypedEncoder
   , makeEncoder, encoderFor, exerciseEncoder
   , (<?>), encodeTyped
     -- * Decoder datatype
   , Decoder, TypedDecoder
   , makeDecoder, decoderFor
   , split, symbol, setInput
     -- re-export
   , module Export
   ) where

import Control.Applicative as Export hiding (Const)
import Control.Arrow as Export
import Control.Monad
import Data.Maybe
import Data.Monoid as Export
import Data.Semigroup as Sem
import Ideas.Common.Library hiding (exerciseId, symbol)
import Ideas.Encoding.Options
import Ideas.Encoding.Request
import Ideas.Service.FeedbackScript.Parser (Script)
import Ideas.Service.Types
import Ideas.Text.JSON hiding (String)
import Ideas.Text.Latex
import Ideas.Text.XML
import Test.QuickCheck.Random
import qualified Control.Category as C
import qualified Ideas.Common.Rewriting.Term as Term
import qualified Ideas.Text.JSON as JSON

-------------------------------------------------------------------
-- Converter type class

class Converter f where
   fromExercise :: (Exercise a -> t) -> f a s t
   fromOptions  :: (Options -> t) -> f a s t
   run :: Monad m => f a s t -> Exercise a -> Options -> s -> m t

getExercise :: Converter f => f a s (Exercise a)
getExercise = fromExercise id

getBaseUrl :: Converter f => f a s String
getBaseUrl = fromOptions (fromMaybe "http://ideas.cs.uu.nl/" . baseUrl)

getQCGen :: Converter f => f a s QCGen
getQCGen = fromOptions (fromMaybe (mkQCGen 0) . qcGen)

getScript :: Converter f => f a s Script
getScript = fromOptions script

getRequest :: Converter f => f a s Request
getRequest = fromOptions request

withExercise :: (Converter f, Monad (f a s)) => (Exercise a -> f a s t) -> f a s t
withExercise = (getExercise >>=)

withOpenMath :: (Converter f, Monad (f a s)) => (Bool -> f a s t) -> f a s t
withOpenMath = (fmap useOpenMath getRequest >>=)

withJSONTerm :: (Converter f, Monad (f a s)) => (Bool -> f a s t) -> f a s t
withJSONTerm = (fmap useJSONTerm getRequest >>=)

(//) :: (Converter f, Monad (f a s2)) => f a s t -> s -> f a s2 t
p // a = do
   opts <- fromOptions id
   ex   <- getExercise
   run p ex opts a

-------------------------------------------------------------------
-- JSON terms

jsonProperty :: Id
jsonProperty = describe "Support for JSON encoding" $ newId "json"

hasJSONView :: Exercise a -> Maybe (View JSON a)
hasJSONView = getPropertyF jsonProperty

addJSONView :: View JSON a -> Exercise a -> Exercise a
addJSONView = setPropertyF jsonProperty

jsonEncoding :: InJSON a => Exercise a -> Exercise a
jsonEncoding = addJSONView (makeView fromJSON toJSON)

termToJSON :: Term -> JSON
termToJSON term =
   case term of
      TVar s    -> JSON.String s
      TCon s []
         | s == trueSymbol  -> Boolean True
         | s == falseSymbol -> Boolean False
         | s == nullSymbol  -> Null
      TCon s ts
         | s == objectSymbol -> Object (f ts)
         | otherwise -> Object [("_apply", Array (JSON.String (show s):map termToJSON ts))]
      TList xs  -> Array (map termToJSON xs)
      TNum n    -> Number (I n)
      TFloat d  -> Number (D d)
      TMeta n   -> Object [("_meta", Number (I (toInteger n)))]
 where
   f [] = []
   f (TVar s:x:xs) = (s, termToJSON x) : f xs
   f _ = error "termToJSON"

jsonToTerm :: JSON -> Term
jsonToTerm json =
   case json of
      Number (I n)  -> TNum n
      Number (D d)  -> TFloat d
      JSON.String s -> TVar s
      Boolean b     -> Term.symbol  (if b then trueSymbol else falseSymbol)
      Array xs      -> TList (map jsonToTerm xs)
      Object [("_meta", Number (I n))] -> TMeta (fromInteger n)
      Object [("_apply", Array (JSON.String s:xs))] -> TCon (newSymbol s) (map jsonToTerm xs)
      Object xs     -> TCon objectSymbol (concatMap f xs)
      Null          -> Term.symbol nullSymbol
 where
   f (s, x) = [TVar s, jsonToTerm x]

nullSymbol, objectSymbol :: Symbol
nullSymbol   = newSymbol "null"
objectSymbol = newSymbol "object"

-------------------------------------------------------------------
-- Latex support

latexProperty :: Id
latexProperty = describe "Support for LaTeX encoding" $ newId "latex"

newtype F a = F { unF :: a -> Latex }

getF :: Exercise a -> Maybe (F a)
getF = getPropertyF latexProperty

hasLatexEncoding :: Exercise a -> Bool
hasLatexEncoding = isJust . getF

-- | Uses exercise pretty-printer in case latex encoding is missing.
latexPrinter :: Exercise a -> a -> Latex
latexPrinter ex = maybe (toLatex . prettyPrinter ex) unF (getF ex)

-- | Uses exercise pretty-printer in case latex encoding is missing.
latexPrinterContext :: Exercise a -> Context a -> Latex
latexPrinterContext ex ctx =
   let def = toLatex (prettyPrinterContext ex ctx)
   in fromMaybe def (unF <$> getF ex <*> fromContext ctx)

latexEncoding :: ToLatex a => Exercise a -> Exercise a
latexEncoding = latexEncodingWith toLatex

latexEncodingWith :: (a -> Latex) -> Exercise a -> Exercise a
latexEncodingWith = setPropertyF latexProperty . F

-------------------------------------------------------------------
-- Encoder datatype

newtype Encoder a s t = Enc { runEnc :: (Exercise a, Options) -> s -> Error t }

type TypedEncoder a = Encoder a (TypedValue (Type a))

instance C.Category (Encoder a) where
   id    = arr id
   f . g = Enc $ \xs -> runEnc g xs >=> runEnc f xs

instance Arrow (Encoder a) where
   arr   f = Enc $ \_ -> return . f
   first f = Enc $ \xs (a, b) -> runEnc f xs a >>= \c -> return (c, b)

instance Functor (Encoder a s) where
   fmap f p = Enc $ \xs s -> f <$> runEnc p xs s

instance Applicative (Encoder a s) where
   pure a  = Enc $ \_ _ -> return a
   p <*> q = Enc $ \xs s -> do
      f <- runEnc p xs s
      f <$> runEnc q xs s

instance Alternative (Encoder a s) where
   empty = fail "Encoder: emptu"
   p <|> q = Enc $ \xs s ->
      runEnc p xs s <|> runEnc q xs s

instance Monad (Encoder a s) where
   return   = pure
   fail s   = Enc $ \_ _ -> fail s
   p >>= f  = Enc $ \xs s -> do
      a <- runEnc p xs s
      runEnc (f a) xs s

instance MonadPlus (Encoder a s) where
   mzero = fail "Encoder: mzero"
   mplus = (<|>)

instance Converter Encoder where
   fromExercise f = Enc $ \(ex, _) _ -> return (f ex)
   fromOptions  f = Enc $ \(_, opts) _ -> return (f opts)
   run f ex opts  = runErrorM . runEnc f (ex, opts)

instance Sem.Semigroup t => Sem.Semigroup (Encoder a s t) where
   (<>) = liftA2 (<>)

instance Monoid t => Monoid (Encoder a s t) where
   mempty  = pure mempty
   mappend = liftA2 mappend

instance BuildXML t => BuildXML (Encoder a s t) where
   n .=. s   = pure (n .=. s)
   unescaped = pure . unescaped
   builder   = pure . builder
   tag       = fmap . tag

makeEncoder :: (s -> t) -> Encoder a s t
makeEncoder = arr

encoderFor :: (s -> Encoder a s t) -> Encoder a s t
encoderFor f = C.id >>= f

exerciseEncoder :: (Exercise a -> s -> t) -> Encoder a s t
exerciseEncoder f = withExercise $ makeEncoder . f

infixr 5 <?>

(<?>) :: (Encoder a t b, Type a1 t) -> Encoder a (TypedValue (Type a1)) b
                                    -> Encoder a (TypedValue (Type a1)) b
(p, t) <?> q = do
   val ::: tp <- makeEncoder id
   case equal tp t of
      Just f -> p // f val
      Nothing -> q

encodeTyped :: Encoder st t b -> Type a t -> Encoder st (TypedValue (Type a)) b
encodeTyped p t = (p, t) <?> fail "Types do not match"

-------------------------------------------------------------------
-- Decoder datatype

newtype Decoder a s t = Dec { runDec :: (Exercise a, Options) -> s -> Error (t, s) }

type TypedDecoder a s = forall t . Type a t -> Decoder a s t

instance Functor (Decoder a s) where
   fmap f p = Dec $ \xs s -> mapFirst f <$> runDec p xs s

instance Applicative (Decoder a s) where
   pure a  = Dec $ \_ s -> return (a, s)
   p <*> q = Dec $ \xs s1 -> do
      (f, s2) <- runDec p xs s1
      mapFirst f <$> runDec q xs s2

instance Alternative (Decoder a s) where
   empty   = fail "Decoder: empty"
   p <|> q = Dec $ \xs s -> runDec p xs s <|> runDec q xs s

instance Monad (Decoder a s) where
   return   = pure
   fail s   = Dec $ \_ _ -> fail s
   p >>= f  = Dec $ \xs s1 -> do
      (a, s2) <- runDec p xs s1
      runDec (f a) xs s2

instance MonadPlus (Decoder a s) where
   mzero = fail "Decoder: mzero"
   mplus = (<|>)

get :: Decoder a s s
get = Dec $ \_ s -> return (s, s)

put :: s -> Decoder a s ()
put s = Dec $ \_ _ -> return ((), s)

instance Converter Decoder where
   fromExercise f = Dec $ \(ex, _) s -> return (f ex, s)
   fromOptions  f = Dec $ \(_, opts) s -> return (f opts, s)
   run f ex opts  = fmap fst . runErrorM . runDec f (ex, opts)

split :: (s -> Either String (t, s)) -> Decoder a s t
split f = get >>= either fail (\(a, s2) -> put s2 >> return a) . f

symbol :: Decoder a [s] s
symbol = split f
 where
   f []     = Left "Empty input"
   f (x:xs) = Right (x, xs)

setInput :: s -> Decoder a s ()
setInput inp = split (\_ -> Right ((), inp))

makeDecoder:: (s -> t) -> Decoder a s t
makeDecoder f = fmap f get

decoderFor :: (s -> Decoder a s t) -> Decoder a s t
decoderFor f = get >>= f

-------------------------------------------------------------------
-- Error monad (helper)

newtype Error a = Error { runError :: Either String a }

instance Functor Error where
   fmap f = Error . fmap f . runError

instance Applicative Error where
   pure    = Error . Right
   p <*> q = Error $
      case (runError p, runError q) of
         (Left s, _)  -> Left s
         (_, Left s)  -> Left s
         (Right f, Right x) -> Right (f x)

instance Alternative Error where
   empty   = Error (Left "empty")
   p <|> q = Error $
      case (runError p, runError q) of
         (Right a, _) -> Right a
         (_, Right a) -> Right a
         (Left s, _)  -> Left s

instance Monad Error where
   fail    = Error . Left
   return  = pure
   m >>= f = Error $ either Left (runError . f) (runError m)

instance MonadPlus Error where
   mzero = fail "mzero"
   mplus = (<|>)

runErrorM :: Monad m => Error a -> m a
runErrorM = either fail return . runError