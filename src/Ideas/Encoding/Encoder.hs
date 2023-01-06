{-# LANGUAGE RankNTypes #-}
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

module Ideas.Encoding.Encoder
   ( -- * Converter type class
     getExercise, getOptions, getRequest
   , withExercise, getBaseUrl, getQCGen, getScript
   , (//), withJSONTerm, withOpenMath
     -- * JSON support
   , hasJSONView, addJSONView, jsonEncoding
   , termToJSON, jsonToTerm
     -- * Latex support
   , hasLatexEncoding, latexPrinter, latexPrinterContext
   , latexEncoding, latexEncodingWith
     -- * Encoder datatype
   , EncoderX, TypedEncoder
   , (<?>), encodeTyped
     -- * Decoder datatype
   , DecoderX, TypedDecoder
   ) where

import Control.Monad.Reader
import Data.Maybe
import Ideas.Common.Library
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

getExercise :: DecoderX a err s (Exercise a)
getExercise = reader fst

getOptions :: DecoderX a err s Options
getOptions = reader snd

getRequest :: DecoderX a err s Request
getRequest = request <$> getOptions

withExercise :: (Exercise a -> DecoderX a err s t) -> DecoderX a err s t
withExercise = (getExercise >>=)

getBaseUrl :: DecoderX a err s String
getBaseUrl = fromMaybe "https://ideas.science.uu.nl/" . baseUrl <$> getOptions

getQCGen :: DecoderX a err s QCGen
getQCGen = fromMaybe (mkQCGen 0) . qcGen <$> getOptions

getScript :: DecoderX a err s Script
getScript = script <$> getOptions

withOpenMath :: (Bool -> DecoderX a err s t) -> DecoderX a err s t
withOpenMath = (fmap useOpenMath getRequest >>=)

withJSONTerm :: (Bool -> DecoderX a err s t) -> DecoderX a err s t
withJSONTerm = (fmap useJSONTerm getRequest >>=)

(//) :: Decoder env err s a -> s -> Decoder env err s2 a
p // a = do
   env  <- ask
   either throwError return (evalDecoder p env a)

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
      TNum n    -> Integer n
      TFloat d  -> Double d
      TMeta n   -> Object [("_meta", Integer (toInteger n))]
 where
   f [] = []
   f (TVar s:x:xs) = (s, termToJSON x) : f xs
   f _ = error "termToJSON"

jsonToTerm :: JSON -> Term
jsonToTerm json =
   case json of
      Integer n     -> TNum n
      Double d      -> TFloat d
      JSON.String s -> TVar s
      Boolean b     -> Term.symbol  (if b then trueSymbol else falseSymbol)
      Array xs      -> TList (map jsonToTerm xs)
      Object [("_meta", Integer n)] -> TMeta (fromInteger n)
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

type EncoderX a = Encoder (Exercise a, Options) String

type TypedEncoder a b = TypedValue (Type a) -> EncoderX a b

infixr 5 <?>

(<?>) :: (t -> EncoderX a b, Type a t) -> TypedEncoder a b -> TypedEncoder a b
((p, t) <?> q) tv@(val ::: tp) =
   case equal tp t of
      Just f  -> p (f val)
      Nothing -> q tv

encodeTyped :: (t -> EncoderX a b) -> Type a t -> TypedEncoder a b
encodeTyped p t1 tv@(_ ::: t2) = ((p, t1) <?> (\_ -> errorStr ("Types do not match: " ++ show t1 ++ " and " ++ show t2))) tv

-------------------------------------------------------------------
-- Decoder datatype

type DecoderX a = Decoder (Exercise a, Options)

type TypedDecoder a s = forall t . Type a t -> DecoderX a String s t