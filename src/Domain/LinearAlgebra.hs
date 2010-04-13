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
module Domain.LinearAlgebra
   ( module Domain.LinearAlgebra.Matrix
   , module Domain.LinearAlgebra.Parser
   , module Domain.LinearAlgebra.MatrixRules
   , module Domain.LinearAlgebra.EquationsRules
   , module Domain.LinearAlgebra.Strategies
   , module Domain.LinearAlgebra.LinearSystem
   , module Domain.LinearAlgebra.Exercises
   ) where
   
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.Parser
import Domain.LinearAlgebra.MatrixRules
import Domain.LinearAlgebra.EquationsRules hiding (changeCover, findIndexM)
import Domain.LinearAlgebra.Strategies
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.Exercises