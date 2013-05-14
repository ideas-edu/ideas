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
-----------------------------------------------------------------------------
module Ideas.Service.RulesInfo
   ( rulesInfoXML, rewriteRuleToFMP, collectExamples, ExampleMap, rulesInfoType
   ) where

import Ideas.Common.Library
import Ideas.Common.Utils (Some(..))
import Control.Monad
import Data.Char
import Ideas.Service.OpenMathSupport (toOMOBJ)
import Ideas.Service.Types
import Ideas.Text.OpenMath.FMP
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML hiding (name)
import qualified Data.Map as M

rulesInfoXML :: Exercise a -> (a -> XMLBuilder) -> XMLBuilder
rulesInfoXML ex enc = forM_ (ruleset ex) $ \r ->

   element "rule" $ do
      "name"        .=. showId r
      "buggy"       .=. f (isBuggy r)
      "rewriterule" .=. f (isRewriteRule r)
      -- More information
      let descr = description r
          -- to do: rules should carry descriptions
          txt   = if null descr then showId r else descr
      unless (null txt) $
         element "description" $ text txt
      forM_ (getRefs r) $ \(Some a) -> 
         element "argument" (text (show a))
      forM_ (ruleSiblings r) $ \s ->
         element "sibling" $ text $ showId s
      -- FMPs and CMPs
      forM_ (getRewriteRules (transformation r)) $ \(Some rr) -> do
         let ok  = not $ isBuggy r
             fmp = rewriteRuleToFMP ok rr
         case showRewriteRule ok rr of
            Nothing -> return ()
            Just s  -> element "CMP" (text s)
         element "FMP" $
            builder (omobj2xml (toObject fmp))
      -- Examples
      let pairs = M.findWithDefault [] (getId r) exampleMap
      forM_ (take 3 pairs) $ \(a, b) ->
         element "example" $ do
            enc a
            enc b
 where
   f          = map toLower . show
   exampleMap = collectExamples ex

rewriteRuleToFMP :: Bool -> RewriteRule a -> FMP
rewriteRuleToFMP sound r
   | sound     = eqFMP    a b
   | otherwise = buggyFMP a b
 where
   a :~> b = fmap toOMOBJ (ruleSpecTerm r)

type ExampleMap a = M.Map Id [(a, a)]

collectExamples :: Exercise a -> ExampleMap a
collectExamples ex = foldr (add . snd) M.empty (examples ex)
 where
   add a m = let tree = derivationTree False (strategy ex) (inContext ex a)
                 f Nothing = m
                 f (Just d) = foldr g m (triples d)
                 g (x, (r, _), y) = 
                    case fromContextWith2 (,) x y of
                       Just p  -> M.insertWith (++) (getId r) [p]
                       Nothing -> id
             in f (derivation tree)

rulesInfoType :: Type a ()
rulesInfoType = Tag "RulesInfo" Unit