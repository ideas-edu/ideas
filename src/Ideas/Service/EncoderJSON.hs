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

jsonEncoder :: MonadPlus m => Exercise a -> Encoder (Type a) (m JSON)
jsonEncoder ex = jsonEncode (String . prettyPrinter ex)

jsonEncode :: MonadPlus m => (a -> JSON) -> Encoder (Type a) (m JSON)
jsonEncode enc tv@(val ::: tp)
   | length (tupleList tv) > 1 =
        liftM jsonTuple (mapM (jsonEncode enc) (tupleList tv))
   | otherwise =
        case tp of
           Iso p t   -> jsonEncode enc (to p val ::: t)
           t1 :|: t2 -> case val of
              Left  x -> jsonEncode enc (x ::: t1)
              Right y -> jsonEncode enc (y ::: t2)
           
           Pair t1 t2 -> do
              x <- jsonEncode enc (fst val ::: t1)
              y <- jsonEncode enc (snd val ::: t2)
              return (jsonTuple [x, y])
           List (Const Rule) -> do
              return $ Array $ map ruleShortInfo val
           Tp.Tag s t
              | s `elem` ["elem", "list"] ->
                   jsonEncode enc (val ::: t)
              | s == "Result" -> do
                   conv <- equalM tp typed
                   encodeResult enc (conv val)
              | s == "Exception" -> do
                   f <- equalM t typed
                   fail (f val)
              | s == "Derivation" -> do
                   f <- equalM tp typed
                   encodeDerivation enc (f val)
                 `mplus` do
                   f <- equalM tp typed
                   encodeDerivationText enc (f val)
              | otherwise -> liftM (\b -> Object [(s, b)]) (jsonEncode enc (val ::: t))

           Tp.Unit   -> return Null
           Tp.List t -> liftM Array (mapM (jsonEncode enc . (::: t)) val)
           Tp.Tree t -> liftM treeToJSON (T.mapM (jsonEncode enc . (::: t)) val)
           Const ctp -> jsonEncodeConst enc (val ::: ctp)
 where
   tupleList :: TypedValue (TypeRep f) -> [TypedValue (TypeRep f)]
   tupleList (x ::: Tp.Iso p t)   = tupleList (to p x ::: t)
   tupleList (p ::: Tp.Pair t1 t2) =
      tupleList (fst p ::: t1) ++ tupleList (snd p ::: t2)
   tupleList (x ::: Tag s t)
      | s `elem` ["message"] = tupleList (x ::: t)
   tupleList (ev ::: (t1 :|: t2)) =
      either (\x -> tupleList (x ::: t1)) 
             (\x -> tupleList (x ::: t2)) ev
   tupleList tv = [tv]

   treeToJSON :: Tree JSON -> JSON
   treeToJSON (Node r ts) =
     case r of
       (Array [x, t]) -> Object [ ("rootLabel", x)
                                , ("type", t)
                                , ("subForest", Array $ map treeToJSON ts) ]
       _ -> error "ModeJSON: malformed tree!"

jsonEncodeConst :: MonadPlus m => (a -> JSON) -> Encoder (Const a) (m JSON)
jsonEncodeConst enc (val ::: tp) =
   case tp of
      SomeExercise -> case val of
                         Some ex -> return (exerciseInfo ex)
      State     -> return (encodeState enc val)
      Rule      -> return (toJSON (showId val))
      Context   -> liftM enc (fromContext val)
      Location  -> return (toJSON (show val))
      Environment -> return (encodeEnvironment val)
      Text      -> return (toJSON (show val))
      Int       -> return (toJSON val)
      Bool      -> return (toJSON val)
      Tp.String -> return (toJSON val)
      _         -> fail $ "Type " ++ show tp ++ " not supported in JSON"

--------------------------

-- legacy representation
encodeEnvironment :: Environment -> JSON
encodeEnvironment = 
   let f a = Object [(showId a, String (showValue a))]
   in Array . map f . bindings

encodeState :: (a -> JSON) -> State a -> JSON
encodeState f st = Array
   [ String (showId (exercise st))
   , String $ case statePrefixes st of
                 [] -> "NoPrefix"
                 ps -> intercalate ";" $ map show ps
   , f (stateTerm st)
   , encodeContext (stateContext st)
   ]

encodeContext :: Context a -> JSON
encodeContext = Object . map f . bindings
 where
   f a = (showId a, String $ showValue a)

encodeDerivation :: MonadPlus m => (a -> JSON) -> Derivation (Rule (Context a), Environment) (Context a) -> m JSON
encodeDerivation enc d = 
   let xs = [ (s, a) | (_, s, a) <- triples d ]
   in jsonEncode enc (xs ::: typed)

encodeDerivationText :: MonadPlus m => (a -> JSON) -> Derivation String (Context a) -> m JSON
encodeDerivationText enc d = 
   let xs = [ (s, a) | (_, s, a) <- triples d ]
   in jsonEncode enc (xs ::: typed)

encodeResult :: MonadPlus m => (a -> JSON) -> Result a -> m JSON
encodeResult enc result =
   case result of
      -- SyntaxError _ -> [("result", String "SyntaxError")]
      Buggy rs      -> return $ Object [("result", String "Buggy"), ("rules", Array $ map (String . showId) rs)]
      NotEquivalent -> return $ Object [("result", String "NotEquivalent")]
      Ok rs st      -> do
         json <- jsonEncode enc (st ::: typed)
         return $ Object [("result", String "Ok"), ("rules", Array $ map (String . showId) rs), ("state", json)]
      Detour rs st  -> do
         json <- jsonEncode enc (st ::: typed)
         return $ Object [("result", String "Detour"), ("rules", Array $ map (String . showId) rs), ("state", json)]
      Unknown st    -> do
         json <- jsonEncode enc (st ::: typed)
         return $ Object [("result", String "Unknown"), ("state", json)]

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