-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
--
-----------------------------------------------------------------------------

module Domain.Programming.Substitute where

import Data.Data
import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Map
import Domain.Programming.AlphaRenaming
import Domain.Programming.Helium
import Domain.Programming.HeliumRules ()

class AlphaRename a where
  alphaRename :: a -> a

instance AlphaRename Expression where
  alphaRename expr = (\(_, e, _, _) -> e) $ sem_Expression expr [] empty []

-- (\x -> x + let x = 3 in x)
subst :: (AlphaRename a, Data a) => (Expression, Expression) -> a -> a
subst (x, y) expr = transformBi f $ expr
  where
    f e | e == x    = y -- assume ranges are removed
        | otherwise = e

substAll :: (AlphaRename a, Data a) => [(Expression, Expression)] -> a -> a
substAll = flip $ foldr subst