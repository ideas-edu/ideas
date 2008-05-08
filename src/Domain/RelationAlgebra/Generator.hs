-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.RelationAlgebra.Generator where

import Domain.RelationAlgebra.Formula
import Domain.RelationAlgebra.Rules -- only for checks
import Common.Apply
import Common.Transformation
import Control.Monad
import Test.QuickCheck 

instance Arbitrary RelAlg where
   arbitrary = sized arbRelAlg
   coarbitrary term =
      case term of
         Var x    -> variant 0 . coarbitrary x
         p :.:  q -> variant 1 . coarbitrary p . coarbitrary q
         p :+:  q -> variant 2 . coarbitrary p . coarbitrary q       
         p :&&: q -> variant 3 . coarbitrary p . coarbitrary q       
         p :||: q -> variant 4 . coarbitrary p . coarbitrary q       
         Not p    -> variant 5 . coarbitrary p
	 Inv p    -> variant 6 . coarbitrary p  
         U        -> variant 7   
         E        -> variant 8      
   
arbRelAlg :: Int -> Gen RelAlg
arbRelAlg 0 = oneof [liftM Var (oneof $ map return vars), return U, return E]
arbRelAlg n = oneof [ arbRelAlg 0, binop (:.:), binop (:+:), binop (:&&:), binop (:||:)
                    , unop Not, unop Inv 
                    ]
 where
   binop op = liftM2 op rec rec
   unop op  = liftM op rec
   rec      = arbRelAlg (n `div` 2)  

vars :: [String]
vars = ["q", "r", "s"]

checks :: IO ()
checks =
   let f r = do putStr ("[" ++ name r ++ "]   ") >> quickCheck (g r)
       g r a = applicable r a ==> a === applyD r a
   in mapM_ f relAlgRules