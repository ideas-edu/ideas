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
module Domain.Math.Numeric.Tests (main) where

import Common.Classes
import Common.Context
import Common.TestSuite
import Common.View
import Control.Monad
import Data.Maybe
import Domain.Math.Expr
import Domain.Math.Numeric.Generators
import Domain.Math.Numeric.Strategies
import Domain.Math.Numeric.Views
import Test.QuickCheck

main :: TestSuite
main = suite "Numeric tests" $ do

   suite "Correctness numeric views" $ do
      let f s v = forM_ numGenerators $ \g -> do
             addProperty ("idempotence " ++ s) $ propIdempotence g v
             addProperty ("soundness " ++ s)   $ propSoundness semEqDouble g v
      f "integer view"          integerView
      f "rational view"         rationalView
      f "integer normal form"   integerNF
      f "rational normal form"  rationalNF
      f "rational relaxed form" rationalRelaxedForm

   suite "Normal forms" $ do
      let f s v = forM_ numGenerators $ \g ->
             addProperty s $ propNormalForm g v
      f "integer normal form" integerNF
    -- f rationalNF -- no longer a normal form

   suite "Correctness generators" $ do
      let f s g v = addProperty s $ forAll (sized g) (`belongsTo` v)
      f "integer" integerGenerator integerView
      f "rational" rationalGenerator rationalView
      f "ratio expr" ratioExprGen rationalNF
      f "ratio expr nonzero" ratioExprGenNonZero rationalNF

   suite "View relations" $ do
      let va .>. vb = forM_ numGenerators $ \g -> 
             addProperty "" $ forAll g $ \a -> 
                not (a `belongsTo` va) || a `belongsTo` vb
      integerNF .>. integerView
      rationalNF .>. rationalRelaxedForm
      rationalRelaxedForm .>. rationalView
      integerNF .>. rationalNF
      integerView .>. rationalView

   suite "Pre/post conditions strategies" $ do
      let f l s pre post = forM_ numGenerators $ \g -> 
             addProperty l $ forAll g $ \a ->
                let run = fromMaybe a . fromContext . applyD s 
                        . newContext emptyEnv . termNavigator
                in not (a `belongsTo` pre) || run a `belongsTo` post
      f "natural"  naturalStrategy  integerView  integerNF
      f "integer"  integerStrategy  integerView  integerNF
      f "rational" rationalStrategy rationalView rationalNF
      f "fraction" fractionStrategy rationalView rationalNF

numGenerators :: [Gen Expr]
numGenerators = map sized 
   [ integerGenerator, rationalGenerator
   , ratioExprGen, ratioExprGenNonZero, numGenerator
   ]
   
semEqDouble :: Expr -> Expr -> Bool
semEqDouble a b = 
   case (match doubleView a, match doubleView b) of
      (Just x, Just y)   -> x ~= y
      (Nothing, Nothing) -> True
      _                  -> False
 where
   delta = 0.0001
 
   (~=) :: Double -> Double -> Bool
   x ~= y = abs x < delta || abs y < delta || abs (1 - (x/y)) < delta