-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.RulesInfo 
   ( rulesInfoXML, rewriteRuleToFMP, collectExamples, rulesInfoType
   ) where

import Common.Library
import Common.Utils (Some(..))
import Data.Char
import Control.Monad
import Text.OpenMath.Object
import Text.OpenMath.FMP
import Text.XML hiding (name)
import Service.ExercisePackage (termToOMOBJ)
import Service.Types
import qualified Data.Map as M

rulesInfoXML :: Monad m => Exercise a -> (a -> m XMLBuilder) -> m XMLBuilder
rulesInfoXML ex enc = combine $ forM (ruleset ex) $ \r -> do
   
   let pairs = M.findWithDefault [] (getId r) exampleMap
   examples <- forM (take 3 pairs) $ \(a, b) ->
                  liftM2 (,) (enc a) (enc b)
                     
   return $ element "rule" $ do
      "name"        .=. showId r
      "buggy"       .=. f (isBuggyRule r)
      "rewriterule" .=. f (isRewriteRule r)
      -- More information
      let descr = description r
          -- to do: rules should carry descriptions 
          txt   = if null descr then (showId r) else descr 
      unless (null txt) $
         element "description" $ text txt
      forM_ (ruleGroups r) $ \s -> 
         element "group" $ text s
      forM_ (ruleSiblings r) $ \s -> 
         element "sibling" $ text $ showId s
      -- FMPs and CMPs
      forM_ (getRewriteRules r) $ \(Some rr, b) -> do
         let fmp = rewriteRuleToFMP b rr
         case showRewriteRule b rr of
            Nothing -> return ()
            Just s  -> element "CMP" (text s)
         element "FMP" $ 
            builder (omobj2xml (toObject fmp))
      -- Examples
      forM_ examples $ \(a, b) ->
         element "example" (a >> b)
 where
   f          = map toLower . show
   exampleMap = collectExamples ex
   combine    = liftM sequence_
   
rewriteRuleToFMP :: Bool -> RewriteRule a -> FMP
rewriteRuleToFMP sound r 
   | sound     = eqFMP    a b
   | otherwise = buggyFMP a b 
 where
   a :~> b = fmap termToOMOBJ (rulePair r 0)
              
collectExamples :: Exercise a -> M.Map Id [(a, a)]
collectExamples ex = foldr add M.empty (examples ex)
 where
   add a m = let tree = derivationTree (strategy ex) (inContext ex a)
                 f Nothing = m
                 f (Just d) = foldr g m (zip3 (terms d) (steps d) (drop 1 (terms d)))
                 g (a, r, b) = M.insertWith (++) (getId r) (liftM2 (,) (fromContext a) (fromContext b))
             in f (derivation tree) 

rulesInfoType :: Type a ()
rulesInfoType = useSynonym (typeSynonym "RulesInfo" id id Unit)