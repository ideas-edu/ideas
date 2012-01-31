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
-- Converting a strategy to XML, and the other way around.
--
-----------------------------------------------------------------------------
module Service.StrategyInfo (strategyToXML, xmlToStrategy) where

import Common.Library
import Common.Strategy.Abstract
import Common.Strategy.Core
import Common.Utils (readInt)
import Control.Monad
import Data.Char
import Data.Maybe
import Text.XML

-----------------------------------------------------------------------
-- Strategy to XML

strategyToXML :: IsStrategy f => f a -> XML
strategyToXML = coreToXML . toCore . toStrategy

infoToXML :: LabelInfo -> XMLBuilder
infoToXML info = do
   "name" .=. showId info
   when (removed   info) ("removed"   .=. "true")
   when (collapsed info) ("collapsed" .=. "true")
   when (hidden    info) ("hidden"    .=. "true")

coreToXML :: Core LabelInfo a -> XML
coreToXML core = makeXML "label" $
   case core of
      Label l a -> infoToXML l >> coreBuilder infoToXML a
      _         -> coreBuilder infoToXML core

coreBuilder :: HasId l => (l -> XMLBuilder) -> Core l a -> XMLBuilder
coreBuilder f = rec
 where
   rec core =
      case core of
         _ :*:  _  -> asList  "sequence"   isSequence
         _ :|:  _  -> asList  "choice"     isChoice
         _ :|>: _  -> asList  "orelse"     isOrElse
         _ :%: _   -> asList  "interleave" isInterleave
         a :!%: b  -> element "interleft"  (rec a >> rec b)
         Many a    -> element "many"       (rec a)
         Repeat a  -> element "repeat"     (rec a)
         Label l (Rule r) | getId l == getId r 
                   -> element "rule"       (f l)
         Label l a -> element "label"      (f l >> rec a)
         Atomic a  -> element "atomic"     (rec a)
         Rec n a   -> element "rec"        (("var" .=. show n) >> rec a)
         Not a     -> element "not"        (recNot a)
         Rule r    -> element "rule"       ("name" .=. show r)
         Var n     -> element "var"        ("var" .=. show n)
         Succeed   -> tag     "succeed"
         Fail      -> tag     "fail"
    where
      asList s g = element s (mapM_ rec (collect g core))
      recNot = coreBuilder (const (return ()))

collect :: (a -> Maybe (a, a)) -> a -> [a]
collect f = ($ []) . rec
 where rec a = maybe (a:) (\(x, y) -> rec x . rec y) (f a)

isSequence :: Core l a -> Maybe (Core l a, Core l a)
isSequence (a :*: b) = Just (a, b)
isSequence _ = Nothing

isChoice :: Core l a -> Maybe (Core l a, Core l a)
isChoice (a :|: b) = Just (a, b)
isChoice _ = Nothing

isOrElse :: Core l a -> Maybe (Core l a, Core l a)
isOrElse (a :|>: b) = Just (a, b)
isOrElse _ = Nothing

isInterleave :: Core l a -> Maybe (Core l a, Core l a)
isInterleave (a :%: b) = Just (a, b)
isInterleave _ = Nothing

-----------------------------------------------------------------------
-- XML to strategy

xmlToStrategy :: Monad m => (String -> Maybe (Rule a)) ->  XML -> m (Strategy a)
xmlToStrategy f = liftM fromCore . readStrategy xmlToInfo g
 where
   g info = case f (showId info) of
               Just r  -> return r
               Nothing -> fail $ "Unknown rule: " ++ showId info

xmlToInfo :: Monad m => XML -> m LabelInfo
xmlToInfo xml = do
   n <- findAttribute "name" xml
   let boolAttr s = fromMaybe False (findBool s xml)
   return (makeInfo n)
      { removed   = boolAttr "removed"
      , collapsed = boolAttr "collapsed"
      , hidden    = boolAttr "hidden"
      }

findBool :: Monad m => String -> XML -> m Bool
findBool attr xml = do
   s <- findAttribute attr xml
   case map toLower s of
      "true"  -> return True
      "false" -> return False
      _       -> fail "not a boolean"

readStrategy :: Monad m => (XML -> m l) -> (l -> m (Rule a)) -> XML -> m (Core l a)
readStrategy toLabel findRule xml = do
   xs <- mapM (readStrategy toLabel findRule) (children xml)
   let s = name xml
   case lookup s table of
      Just f  -> f s xs
      Nothing ->
         fail $ "Unknown strategy combinator " ++ show s
 where
   buildSequence _ xs
      | null xs   = return Succeed
      | otherwise = return (foldr1 (:*:) xs)
   buildChoice _ xs
      | null xs   = return Fail
      | otherwise = return (foldr1 (:|:) xs)
   buildOrElse _ xs
      | null xs   = return Fail
      | otherwise = return (foldr1 (:|>:) xs)
   buildInterleave _ xs
      | null xs   = return Succeed
      | otherwise = return (foldr1 (:%:) xs)
   buildLabel x = do
      info <- toLabel xml
      return (Label info x)
   buildRule = do
      info <- toLabel xml
      r    <- findRule info
      return (Label info (Rule r))
   buildRec x = do
      s <- findAttribute "var" xml
      i <- maybe (fail "var: not an int") return (readInt s)
      return (Rec i x)
   buildVar = do
      s <- findAttribute "var" xml
      i <- maybe (fail "var: not an int") return (readInt s)
      return (Var i)

   comb0 a _ [] = return a
   comb0 _ s _  = fail $ "Strategy combinator " ++ s ++ "expects 0 args"

   comb1 f _ [x] = return (f x)
   comb1 _ s _   = fail $ "Strategy combinator " ++ s ++ "expects 1 arg"

   join2 f g a b = join (f g a b)

   table =
      [ ("sequence",   buildSequence)
      , ("choice",     buildChoice)
      , ("orelse",     buildOrElse)
      , ("interleave", buildInterleave)
      , ("many",       comb1 Many)
      , ("repeat",     comb1 Repeat)
      , ("label",      join2 comb1 buildLabel)
      , ("atomic",     comb1 Atomic)
      , ("rec",        join2 comb1 buildRec)
      , ("not",        comb1 (Not . noLabels))
      , ("rule",       join2 comb0 buildRule)
      , ("var",        join2 comb0 buildVar)
      , ("succeed",    comb0 Succeed)
      , ("fail",       comb0 Fail)
      ]