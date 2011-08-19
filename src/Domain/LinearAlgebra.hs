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
module Domain.LinearAlgebra (module Export) where
   
import Domain.LinearAlgebra.Matrix as Export
import Domain.LinearAlgebra.Parser as Export
import Domain.LinearAlgebra.MatrixRules as Export
import Domain.LinearAlgebra.EquationsRules as Export hiding (changeCover, findIndexM)
import Domain.LinearAlgebra.Strategies as Export
import Domain.LinearAlgebra.LinearSystem as Export
import Domain.LinearAlgebra.Exercises as Export