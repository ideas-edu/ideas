{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- Copyright 2012, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  johan.jeuring@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Data.TestingClassLaws where
   
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Interval(Endpoint(..))
import Domain.Math.Data.OrList
import Domain.Math.Data.WithBool
import Domain.Math.Data.SquareRoot
import Domain.Math.Data.Relation

import Test.ClassLaws
import Control.Monad.Laws

------------------------------------------------------------------------------

instance FunctorLaws Polynomial

type instance Param (Polynomial a) = ()

instance (Eq a, Show a, Num a) => TestEqual (Polynomial a) where
  testEqual p _ = testEq (==) p

testFunctorPolynomial
  = do quickLawCheck   (undefined::FunctorLaw1 Int Polynomial)
       quickFLawCheck  (undefined::FunctorLaw2 Int Int Int Polynomial)

------------------------------------------------------------------------------

instance FunctorLaws Endpoint

type instance Param (Endpoint a) = ()

instance (Eq a, Show a, Num a) => TestEqual (Endpoint a) where
  testEqual p _ = testEq (==) p

testFunctorEndpoint
  = do quickLawCheck   (undefined::FunctorLaw1 Float Endpoint)
       quickFLawCheck  (undefined::FunctorLaw2 Float Float Float Endpoint)

------------------------------------------------------------------------------

instance FunctorLaws OrList

type instance Param (OrList a) = ()

instance (Eq a, Show a, Num a) => TestEqual (OrList a) where
  testEqual p _ = testEq (==) p

testFunctorOrList
  = do quickLawCheck   (undefined::FunctorLaw1 Float OrList)
       quickFLawCheck  (undefined::FunctorLaw2 Float Float Float OrList)

------------------------------------------------------------------------------

instance FunctorLaws Relation

type instance Param (Relation a) = ()

instance (Eq a, Show a, Num a) => TestEqual (Relation a) where
  testEqual p _ = testEq (==) p

testFunctorRelation
  = do quickLawCheck   (undefined::FunctorLaw1 Float Relation)
       quickFLawCheck  (undefined::FunctorLaw2 Float Float Float Relation)

------------------------------------------------------------------------------

instance FunctorLaws SquareRoot

type instance Param (SquareRoot a) = ()

instance (Eq a, Show a, Num a) => TestEqual (SquareRoot a) where
  testEqual p _ = testEq (==) p

testFunctorSquareRoot
  = do quickLawCheck   (undefined::FunctorLaw1 Float SquareRoot)
       quickFLawCheck  (undefined::FunctorLaw2 Float Float Float SquareRoot)

------------------------------------------------------------------------------

instance FunctorLaws WithBool

type instance Param (WithBool a) = ()

instance (Eq a, Show a, Num a) => TestEqual (WithBool a) where
  testEqual p _ = testEq (==) p

testFunctorWithBool
  = do quickLawCheck   (undefined::FunctorLaw1 Int WithBool)
       quickFLawCheck  (undefined::FunctorLaw2 Int Int Int WithBool)

------------------------------------------------------------------------------

testFunctorAll = do testFunctorPolynomial
                    testFunctorEndpoint
                    testFunctorOrList
                    testFunctorRelation
                    testFunctorSquareRoot
                    testFunctorWithBool