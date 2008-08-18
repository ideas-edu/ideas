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
import Domain.RelationAlgebra.Equivalence
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
arbRelAlg 0 = frequency [(8, liftM Var (oneof $ map return vars)), (1, return U), (1, return E)]
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
       g r a = applicable r a ==> a `isEquivalent` applyD r a
   in mapM_ f relAlgRules
   
-------------------------------------------------------------------
-- Templates

template1 x y z = x :||: (y :&&: z)
template2 x y z = Not(x :&&: (y :||: z))
template3 x y z = Inv(x :||: (y :&&: z))
template4 x y z = Inv (Not(x :&&: (y :||: z)))
template5 x y z v = Inv (Not((x :||: v) :&&: (y :||: z))) 
template6 mp a b mq = f1 (f2 (a :&&: b))
 where f1 x = maybe x (:.: x) mp
       f2 x = maybe x (x :.:) mq 
template7 x y z = x :.: (y :||:z) 
template8 x y z = x :||:(Not(Inv(y :.: z) :&&: Not(Inv(y) :.: Inv(z))))

gen1 :: Gen RelAlg
gen1 = liftM3 template1 (arbInvNotMol 4) (arbInvNotMol 4) (arbInvNotMol 4)
gen2 :: Gen RelAlg
gen2 = liftM3 template2 (arbInvNotMol 2) (arbInvNotMol 2) (arbInvNotMol 2)
gen3 :: Gen RelAlg
gen3 = liftM3 template3 (arbInvNotMol 2) (arbInvNotMol 2) (arbInvNotMol 2)
gen4 :: Gen RelAlg
gen4 = liftM3 template4 (arbInvNotMol 2) (arbInvNotMol 2) (arbInvNotMol 2)
gen5 :: Gen RelAlg
gen5 = liftM4 template5 (arbInvNotMol 2) (arbInvNotMol 2) (arbInvNotMol 2) (arbInvNotMol 2)
hulpgen1 :: Int -> Gen RelAlg
hulpgen1 n = liftM4 template6 (arbMaybeInvNotMol n) arbVar arbVar (arbMaybeInvNotMol n)
gen6 :: Gen RelAlg
gen6 = liftM3 template1 (hulpgen1 2) (arbInvNotMol 4) (arbInvNotMol 4)
gen7 :: Gen RelAlg
gen7 = liftM3 template1 (arbInvNotMol 4) (hulpgen1 2) (arbInvNotMol 4)
gen8 :: Gen RelAlg
gen8 = liftM3 template2 (arbInvNotMol 4) (hulpgen1 2) (arbInvNotMol 4)
hulpgen2 :: Int -> Gen RelAlg
hulpgen2 n = liftM3 template7 (arbInvNotMol 1) (arbRelAlg n) (arbRelAlg n)
gen9 :: Gen RelAlg
gen9 = liftM3 template8 (hulpgen2 2) (arbInvNotMol 2) (arbInvNotMol 2)


arbInvNotMol :: Int -> Gen RelAlg
arbInvNotMol 0 = frequency [(10, liftM Var (oneof $ map return vars)), (1, return U), (1, return E)]
arbInvNotMol n = frequency [ (1, arbRelAlg 0), (4, binop (:.:)), (4, binop (:+:)), (2, unop Not), (2, unop Inv) ]
 where
   binop op = liftM2 op rec rec
   unop op  = liftM op rec
   rec      = arbInvNotMol (n `div` 2)

arbMaybeInvNotMol :: Int -> Gen (Maybe RelAlg)
arbMaybeInvNotMol n = frequency [(3, liftM Just (arbInvNotMol n)), (1, return Nothing)]

arbVar :: Gen RelAlg
arbVar = liftM Var (oneof $ map return vars)


