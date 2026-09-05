{-# LANGUAGE OverloadedStrings #-}
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
-- Converting a strategy to XML, and the other way around.
--
-----------------------------------------------------------------------------

module Ideas.Encoding.StrategyInfo (strategyToXML) where

import Data.String
import Ideas.Common.Id
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Configuration
import Ideas.Common.Strategy.CyclicTree
import Ideas.Common.Strategy.StrategyTree (StrategyTree)
import Ideas.Text.XML

-----------------------------------------------------------------------
-- Strategy to XML

strategyToXML :: IsStrategy f => f a -> XML
strategyToXML = strategyTreeToXML . toStrategyTree

nameAttr :: Id -> XMLBuilder
nameAttr info = "name" .=. showId info

strategyTreeToXML :: StrategyTree a -> XML
strategyTreeToXML tree = makeXML "label" $
   case isLabel tree of
      Just (l, a) -> nameAttr l <> strategyTreeBuilder a
      _ -> strategyTreeBuilder tree

strategyTreeBuilder :: StrategyTree a -> XMLBuilder
strategyTreeBuilder = builder . fold emptyAlg
   { fNode = \def xs ->
        case xs of
           [x] | isConfigId def
             -> addProperty (fromString (show def)) x
           _ -> makeXML (fromString (show def)) (mconcat (map builder xs))
   , fLeaf = \r ->
        makeXML "rule" ("name" .=. show r)
   , fLabel = \l a ->
        makeXML "label" (nameAttr l <> builder a)
   , fRec = \n a ->
        makeXML "rec" (("var" .=. show n) <> builder a)
   , fVar = \n ->
        makeXML "var" ("var" .=. show n)
   }

addProperty :: Name -> XML -> XML
addProperty n a =
   if getName a `elem` ["label", "rule"]
   then changeAttributes (<> attribute n "true") a
   else a

-----------------------------------------------------------------------
-- XML to strategy

{-
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

findBool :: Monad m => String -> XML -> m Bool
findBool attr xml = do
   s <- findAttribute attr xml
   case map toLower s of
      "true"  -> return True
      "false" -> return False
      _       -> fail "not a boolean"

readStrategy :: Monad m => (XML -> m Id) -> (Id -> m (Rule a)) -> XML -> m (Core a)
readStrategy toLabel findRule xml = error "not implemented" do
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
      | null xs   = return succeedCore
      | otherwise = return (foldr1 (:%:) xs)
   buildLabel x = do
      info <- toLabel xml
      return (Label info x)
   buildRule = do
      info <- toLabel xml
      r    <- findRule info
      return (Label info (Sym r))
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
     -- , ("atomic",     comb1 Atomic)
      , ("rule",       join2 comb0 buildRule)
      , ("var",        join2 comb0 buildVar)
--      , ("succeed",    comb0 Succeed)
      --, ("fail",       comb0 Fail)
      ]

-}