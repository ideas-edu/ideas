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
     getExercise, getBaseUrl, getQCGen, getScript, getRequest
   , withExercise, withOpenMath, withJSONTerm, (//)
     -- * JSON support
   , hasJSONView, addJSONView, jsonEncoding
   , termToJSON, jsonToTerm
     -- * Latex support
   , hasLatexEncoding, latexPrinter, latexPrinterContext
   , latexEncoding, latexEncodingWith
     -- * Encoder datatype
   , Encoder, TypedEncoder
   , encoderFor, exerciseEncoder
   , (<?>), encodeTyped
     -- * Decoder datatype
   , DecoderX, TypedDecoder
   , symbol
   ) where

import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import Ideas.Common.Library hiding (exerciseId, symbol)
import Ideas.Encoding.Options
import Ideas.Encoding.Request
import Ideas.Service.FeedbackScript.Parser (Script)
import Ideas.Service.Types
import Ideas.Text.JSON hiding (String)
import Ideas.Text.Latex
import Ideas.Utils.Decoding 
import Test.QuickCheck.Random
import qualified Ideas.Common.Rewriting.Term as Term
import qualified Ideas.Text.JSON as JSON

-------------------------------------------------------------------
-- Converter type class

getExercise :: DecoderX a s (Exercise a)
getExercise = fromExercise id

getBaseUrl :: DecoderX a s String
getBaseUrl = fromOptions (fromMaybe "http://ideas.cs.uu.nl/" . baseUrl)

getQCGen :: DecoderX a s QCGen
getQCGen = fromOptions (fromMaybe (mkQCGen 0) . qcGen)

getScript :: DecoderX a s Script
getScript = fromOptions script

getRequest :: DecoderX a s Request
getRequest = fromOptions request

withExercise :: (Exercise a -> DecoderX a s t) -> DecoderX a s t
withExercise = (getExercise >>=)

withOpenMath :: (Bool -> DecoderX a s t) -> DecoderX a s t
withOpenMath = (fmap useOpenMath getRequest >>=)

withJSONTerm :: (Bool -> DecoderX a s t) -> DecoderX a s t
withJSONTerm = (fmap useJSONTerm getRequest >>=)

(//) :: Decoder env s a -> s -> Decoder env s2 a
p // a = do
   env  <- ask
   runDecoder p env a

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

type Encoder a s t = DecoderX a s t

type TypedEncoder a s = Encoder a (TypedValue (Type a)) s

encoderFor :: (s -> Encoder a s t) -> Encoder a s t
encoderFor f = get >>= f

exerciseEncoder :: (Exercise a -> s -> t) -> Encoder a s t
exerciseEncoder f = withExercise $ gets . f

infixr 5 <?>

(<?>) :: (Encoder a t b, Type a1 t) -> Encoder a (TypedValue (Type a1)) b
                                    -> Encoder a (TypedValue (Type a1)) b
(p, t) <?> q = do
   val ::: tp <- get
   case equal tp t of
      Just f -> p // f val
      Nothing -> q

encodeTyped :: Encoder st t b -> Type a t -> Encoder st (TypedValue (Type a)) b
encodeTyped p t = (p, t) <?> fail "Types do not match"

-------------------------------------------------------------------
-- Decoder datatype

type DecoderX a = Decoder (Exercise a, Options)

type TypedDecoder a s = forall t . Type a t -> Decoder (Exercise a, Options) s t

fromExercise :: (Exercise a -> t) -> DecoderX a s t
fromExercise f = reader (f . fst)

fromOptions  :: (Options -> t) -> DecoderX a s t
fromOptions  f = reader (f . snd)

symbol :: Decoder env [s] s
symbol = get >>= \list ->
   case list of
      []   -> fail "Empty input"
      x:xs ->
         put xs >> return x