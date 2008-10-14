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
module Domain.RelationAlgebra.Generator (templateGenerator) where

import Domain.RelationAlgebra.Formula
import Common.Rewriting
import Control.Monad
import Test.QuickCheck 
import Common.Exercise

instance Rewrite RelAlg

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

-------------------------------------------------------------------
-- Template generators

templateGenerator :: Int -> Gen RelAlg
templateGenerator n = oneof (map ($ n) [gen1,gen2,gen3,gen4,gen5,gen6,gen7,gen8,gen9])

gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9 :: Int -> Gen RelAlg
gen1 = use3 template1 arbInvNotMol arbInvNotMol arbInvNotMol
gen2 = use3 template2 arbInvNotMol arbInvNotMol arbInvNotMol
gen3 = use3 template3 arbInvNotMol arbInvNotMol arbInvNotMol
gen4 = use3 template4 arbInvNotMol arbInvNotMol arbInvNotMol
gen5 = use4 template5 arbInvNotMol arbInvNotMol arbInvNotMol arbInvNotMol
gen6 = use3 template1 hulpgen1 arbInvNotMol arbInvNotMol
gen7 = use3 template1 arbInvNotMol hulpgen1 arbInvNotMol
gen8 = use3 template2 arbInvNotMol hulpgen1 arbInvNotMol
gen9 = use3 template8 hulpgen2 arbInvNotMol arbInvNotMol

use3 temp f g h   n = liftM3 temp (f n) (g n) (h n)
use4 temp f g h k n = liftM4 temp (f n) (g n) (h n) (k n)

hulpgen1 :: Int -> Gen RelAlg
hulpgen1 n = liftM4 template6 (arbMaybeInvNotMol n) arbVar arbVar (arbMaybeInvNotMol n)

hulpgen2 :: Int -> Gen RelAlg
hulpgen2 n = liftM3 template7 (arbInvNotMol 1) (arbRelAlg n) (arbRelAlg n)

arbInvNotMol :: Int -> Gen RelAlg
arbInvNotMol 0 = frequency [(10, liftM Var (oneof $ map return vars)), (1, return U), (1, return E)]
arbInvNotMol n = frequency [ (10, arbInvNotMol 0), (4, binop (:.:)), (4, binop (:+:)), (2, unop Not), (2, unop Inv) ]
 where
   binop op = liftM2 op rec rec
   unop op  = liftM op rec
   rec      = arbInvNotMol (n `div` 2)

arbMaybeInvNotMol :: Int -> Gen (Maybe RelAlg)
arbMaybeInvNotMol n = frequency [(3, liftM Just (arbInvNotMol n)), (1, return Nothing)]

arbVar :: Gen RelAlg
arbVar = liftM Var (oneof $ map return vars)