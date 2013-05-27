{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Services using JSON notation
--
-----------------------------------------------------------------------------
module Ideas.Service.EncoderJSON (jsonEncoder) where

import Ideas.Common.Library hiding (exerciseId)
import Ideas.Common.Utils (Some(..), distinct)
import Data.List (intercalate)
import Control.Monad
import qualified Data.Traversable as T
import Data.Tree
import Ideas.Service.Evaluator
import Ideas.Service.State
import Ideas.Service.Submit
import Ideas.Service.Types hiding (String)
import Ideas.Text.JSON
import qualified Ideas.Service.Types as Tp

type JSONEncoder a t = EncoderState (a -> JSON) t JSON

jsonEncoder :: JSONEncoder a (TypedValue (Type a))
jsonEncoder = encoderStateFor $ \enc tv@(val ::: tp) -> 
   case tp of
      _ | length (tupleList tv) > 1 ->
         jsonTuple <$> sequence [ jsonEncoder // x | x <- tupleList tv ]
      Iso p t   -> jsonEncoder // (to p val ::: t)
      t1 :|: t2 -> case val of
         Left  x -> jsonEncoder // (x ::: t1)
         Right y -> jsonEncoder // (y ::: t2)
      Pair t1 t2 -> 
         let f x y = jsonTuple [x, y]
         in liftA2 f (jsonEncoder // (fst val ::: t1))
                     (jsonEncoder // (snd val ::: t2))
      List (Const Rule) ->
         pure $ Array $ map ruleShortInfo val
      Tp.Tag s t
         | s == "Result" -> encodeTyped encodeResult
         | s == "Derivation" -> encodeTyped encodeDerivation <+> 
                                encodeTyped encodeDerivationText
         | otherwise -> (\b -> Object [(s, b)]) <$> jsonEncoder // (val ::: t)
      Tp.Unit   -> pure Null
      Tp.List t -> Array <$> sequence [ jsonEncoder // (x ::: t) | x <- val ]
      -- Tp.Tree t -> -- liftM treeToJSON (T.mapM ((jsonEncoder //) . (::: t)) val)
      Const ctp -> jsonEncodeConst // (val ::: ctp)
 where
   tupleList :: TypedValue (TypeRep f) -> [TypedValue (TypeRep f)]
   tupleList (x ::: Tp.Iso p t)   = tupleList (to p x ::: t)
   tupleList (p ::: Tp.Pair t1 t2) =
      tupleList (fst p ::: t1) ++ tupleList (snd p ::: t2)
   tupleList (x ::: Tag s t)
      | s `elem` ["Message"] = tupleList (x ::: t)
   tupleList (ev ::: (t1 :|: t2)) =
      either (\x -> tupleList (x ::: t1)) 
             (\x -> tupleList (x ::: t2)) ev
   tupleList tv = [tv]

jsonEncodeConst :: JSONEncoder a (TypedValue (Const a))
jsonEncodeConst = encoderStateFor $ \encTerm (val ::: tp) ->
   case tp of
      SomeExercise -> case val of
                         Some ex -> pure (exerciseInfo ex)
      State        -> encodeState // val
      Rule         -> pure (toJSON (showId val))
      Context      -> maybe zeroArrow (pure . encTerm) (fromContext val)
      Location     -> pure (toJSON (show val))
      Environment  -> encodeEnvironment // val
      Text         -> pure (toJSON (show val))
      Int          -> pure (toJSON val)
      Bool         -> pure (toJSON val)
      Tp.String    -> pure (toJSON val)
      _ -> fail $ "Type " ++ show tp ++ " not supported in JSON"

--------------------------

-- legacy representation
encodeEnvironment :: JSONEncoder a Environment
encodeEnvironment = encoderFor $ \env -> 
   let f a = Object [(showId a, String (showValue a))]
   in pure $ Array [ f a | a <- bindings env ]

encodeState :: JSONEncoder a (State a)
encodeState = encoderStateFor $ \encTerm st -> 
   let f x = [ String (showId (exercise st))
             , String $ case statePrefixes st of
                           [] -> "NoPrefix"
                           ps -> intercalate ";" $ map show ps
             , encTerm (stateTerm st)
             , x
             ]
   in Array . f <$> encodeContext // stateContext st

encodeContext :: JSONEncoder a (Context a)
encodeContext = encoderFor $ \ctx -> 
   pure $ Object [ (showId a, String $ showValue a) | a <- bindings ctx ]

encodeDerivation :: JSONEncoder a (Derivation (Rule (Context a), Environment) (Context a))
encodeDerivation = encoderFor $ \d ->
   let xs = [ (s, a) | (_, s, a) <- triples d ]
   in jsonEncoder // (xs ::: typed)

encodeDerivationText :: JSONEncoder a (Derivation String (Context a))
encodeDerivationText = encoderFor $ \d ->  
   let xs = [ (s, a) | (_, s, a) <- triples d ]
   in jsonEncoder // (xs ::: typed)

encodeResult :: JSONEncoder a (Result a)
encodeResult = encoderFor $ \result -> Object <$>
   case result of
      -- SyntaxError _ -> [("result", String "SyntaxError")]
      Buggy rs -> pure 
         [ ("result", String "Buggy")
         , ("rules", Array $ map (String . showId) rs)
         ]
      NotEquivalent -> pure
         [ ("result", String "NotEquivalent") ]
      Ok rs st ->
         let f x =
                [ ("result", String "Ok")
                , ("rules", Array $ map (String . showId) rs)
                , ("state", x)
                ]
         in f <$> jsonEncoder // (st ::: typed)
      Detour rs st ->
         let f x = 
                [ ("result", String "Detour")
                , ("rules", Array $ map (String . showId) rs)
                , ("state", x)
                ]
         in f <$> jsonEncoder // (st ::: typed)
      Unknown st ->
         let f x = 
                [ ("result", String "Unknown")
                , ("state", x)
                ]
         in f <$> jsonEncoder // (st ::: typed)

encodeTree :: Tree JSON -> JSON
encodeTree (Node r ts) =
  case r of
    Array [x, t] -> Object 
       [ ("rootLabel", x)
       , ("type", t)
       , ("subForest", Array $ map encodeTree ts) 
       ]
    _ -> error "ModeJSON: malformed tree!"

jsonTuple :: [JSON] -> JSON
jsonTuple xs =
   case mapM f xs of
      Just ys | distinct (map fst ys) -> Object ys
      _ -> Array xs
 where
   f (Object [p]) = Just p
   f _ = Nothing

ruleShortInfo :: Rule a -> JSON
ruleShortInfo r = Object
   [ ("name",        toJSON (showId r))
   , ("buggy",       toJSON (isBuggy r))
   , ("arguments",   toJSON (length (getRefs r)))
   , ("rewriterule", toJSON (isRewriteRule r))
   ]
   
exerciseInfo :: Exercise a -> JSON
exerciseInfo ex = Object 
   [ ("exerciseid", toJSON (showId ex))
   , ("description", toJSON (description ex))
   , ("status", toJSON (show (status ex)))
   ]