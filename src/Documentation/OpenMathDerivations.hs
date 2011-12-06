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
module Documentation.OpenMathDerivations (makeOpenMathDerivations) where

import Common.Library
import Control.Monad
import Data.Maybe
import Service.OpenMathSupport
import Text.OpenMath.Object
import Text.XML

makeOpenMathDerivations :: String -> Exercise a -> IO ()
makeOpenMathDerivations dir ex =
   when (isJust $ hasTermView ex) $ do
      let file = dir ++ "/derivations/" ++ showId ex ++ ".xml"
      putStrLn $ "Generating " ++ file
      writeFile file $
         "<?xml-stylesheet href=\"xsl/ideas.xsl\" type=\"text/xsl\" ?>\n" ++
         show (derivationsXML ex)

derivationsXML :: Exercise a -> XML
derivationsXML ex = makeXML "derivations" $ do
   "title" .=. showId ex
   forM_ (zip [1::Int ..] (examples ex)) $ \(i, (_, a)) ->
      element "derivation" $ do
         "title" .=. show i
         let der = derivationDiffEnv (defaultDerivation ex a)
         derivationM f g der
 where
   f ((r, _), _) = element "step" $ text (showId r)
   g a = case fromContext a >>= toOpenMath ex of
            Just om -> builder (omobj2xml om)
            Nothing -> return ()