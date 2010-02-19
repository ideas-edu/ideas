-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Generating XML for a strategy
--
-----------------------------------------------------------------------------
module Common.Strategy.ToXML (strategyToXML) where

import Control.Monad
import Common.Strategy.Core
import Common.Strategy.Abstract
import Text.XML
--import Domain.Math.Polynomial.Strategies

-- for testing
{-
main :: IO ()
main = do
   let txt = showXML (strategyToXML quadraticStrategy)
   putStrLn txt
   writeFile "quadreq.xml" txt -}

strategyToXML :: IsStrategy f => f a -> XML
strategyToXML = coreToXML . toCore . toStrategy

infoToXML :: LabelInfo -> XMLBuilder
infoToXML info = do
   "name" .=. labelName info
   when (hidden  info) ("hidden"  .=. "true")
   when (skipped info) ("skipped" .=. "true")
   when (folded  info) ("folded"  .=. "true")

coreToXML :: Core LabelInfo a -> XML
coreToXML core = makeXML "strategy" $ 
   case core of
      Label l a -> infoToXML l >> coreBuilder infoToXML a
      _         -> coreBuilder infoToXML core

coreBuilder :: (l -> XMLBuilder) -> Core l a -> XMLBuilder
coreBuilder f = rec
 where
   rec core = 
      case core of
         _ :*:  _  -> asList  "sequence" isSequence
         _ :|:  _  -> asList  "choice"   isChoice
         _ :|>: _  -> asList  "orelse"   isOrElse
         Many a    -> element "many"     (rec a)
         Repeat a  -> element "repeat"   (rec a)
         Label l a -> element "label"    (f l >> rec a)
         Rec n a   -> element "rec"      (("var" .=. show n) >> rec a)
         Not a     -> element "not"      (recNot a)
         Rule l r  -> element "rule"     (maybe ("name" .=. show r) f l)
         Var n     -> element "var"      ("var" .=. show n)
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