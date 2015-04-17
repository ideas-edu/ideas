-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
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
--  $Id$

module Ideas.Encoding.StrategyInfo (strategyToXML, xmlToStrategy) where

import Control.Monad
import Ideas.Common.Library hiding (Remove, Collapse, Hide, (:=))
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Core
import Ideas.Common.Utils (readInt)
import Ideas.Text.XML

-----------------------------------------------------------------------
-- Strategy to XML

strategyToXML :: IsStrategy f => f a -> XML
strategyToXML = coreToXML . toCore . toStrategy

nameAttr :: Id -> XMLBuilder
nameAttr info = "name" .=. showId info

coreToXML :: Core a -> XML
coreToXML core = makeXML "label" $
   case core of
      Label l a -> nameAttr l <> coreBuilder a
      _         -> coreBuilder core

coreBuilder :: Core a -> XMLBuilder
coreBuilder core =
   case core of
      _ :*:  _   -> asList "sequence"   isSequence
      _ :|:  _   -> asList "choice"     isChoice
      _ :>|> _   -> asList "preference" isPreference
      _ :|>: _   -> asList "orelse"     isOrElse
      _ :%: _    -> asList "interleave" isInterleave
      a :@: b    -> tag "alternate" (coreBuilder a <> coreBuilder b)
      r :!~> a   -> tag "atomicprefix" (("name" .=. show r) <> coreBuilder a)
      Label l (Rule r) | getId l == getId r
                 -> tag "rule"       (nameAttr l)
      Label l a  -> tag "label"      (nameAttr l <> coreBuilder a)
      Atomic a   -> tag "atomic"     (coreBuilder a)
      Inits a    -> tag "inits"      (coreBuilder a)
      Not a      -> tag "not"        (coreBuilder a)
      Remove a   -> cfgItem "removed"     (coreBuilder a)
      Collapse a -> cfgItem "collapsed" (coreBuilder a)
      Hide a     -> cfgItem "hidden"      (coreBuilder a)
      Let ds a   -> tag "let"        (decls ds <> coreBuilder a)
      Rule r     -> tag "rule"       ("name" .=. show r)
      Var n      -> tag "var"        ("var" .=. show n)
      Succeed    -> emptyTag "succeed"
      Fail       -> emptyTag "fail"
 where
   asList s g = element s (map coreBuilder (collect g core))
   decls ds   = mconcat [ tag "decl" (("var" .=. show n) <> coreBuilder a)
                        | (n, a) <- ds
                        ]

cfgItem :: String -> XMLBuilder -> XMLBuilder
cfgItem s a =
   case fromBuilder a of
      Just e | name e `elem` ["label", "rule"] ->
         builder e { attributes = attributes e ++ [s := "true"] }
      _      -> tag s a

collect :: (a -> Maybe (a, a)) -> a -> [a]
collect f = ($ []) . rec
 where rec a = maybe (a:) (\(x, y) -> rec x . rec y) (f a)

isSequence :: Core a -> Maybe (Core a, Core a)
isSequence (a :*: b) = Just (a, b)
isSequence _ = Nothing

isChoice :: Core a -> Maybe (Core a, Core a)
isChoice (a :|: b) = Just (a, b)
isChoice _ = Nothing

isPreference :: Core a -> Maybe (Core a, Core a)
isPreference (a :>|> b) = Just (a, b)
isPreference _ = Nothing

isOrElse :: Core a -> Maybe (Core a, Core a)
isOrElse (a :|>: b) = Just (a, b)
isOrElse _ = Nothing

isInterleave :: Core a -> Maybe (Core a, Core a)
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

xmlToInfo :: Monad m => XML -> m Id
xmlToInfo xml = do
   n <- findAttribute "name" xml
   -- let boolAttr s = fromMaybe False (findBool s xml)
   return (newId n)
{-
findBool :: Monad m => String -> XML -> m Bool
findBool attr xml = do
   s <- findAttribute attr xml
   case map toLower s of
      "true"  -> return True
      "false" -> return False
      _       -> fail "not a boolean" -}

readStrategy :: Monad m => (XML -> m Id) -> (Id -> m (Rule a)) -> XML -> m (Core a)
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
      , ("label",      join2 comb1 buildLabel)
      , ("atomic",     comb1 Atomic)
      , ("rule",       join2 comb0 buildRule)
      , ("var",        join2 comb0 buildVar)
      , ("succeed",    comb0 Succeed)
      , ("fail",       comb0 Fail)
      ]