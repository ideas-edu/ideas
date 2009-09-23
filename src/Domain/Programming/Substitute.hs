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
import Data.Generics.Biplate ()
import Data.Generics.PlateData
import Domain.Programming.Helium
import Domain.Programming.HeliumRules ()

-- (\x -> x + let x = 3 in x)
subst :: Data a => (Expression, Expression) -> a -> a
subst (x, y) expr = transformBi f expr
  where
    f e | e == x    = y -- assume ranges are removed
        | otherwise = e

substAll :: Data a => [(Expression, Expression)] -> a -> a
substAll = flip $ foldr subst