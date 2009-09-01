module Domain.Math.Numeric.Tests (main) where

import Common.Apply
import Common.View
import Domain.Math.Expr
import Domain.Math.Numeric.Generators
import Domain.Math.Numeric.Strategies
import Domain.Math.Numeric.Views
import Test.QuickCheck

main :: IO ()
main = do
   putStrLn "*** Correctness numeric views"
   let f v = flip mapM_ numGenerators $ \g -> do
          quickCheck $ propIdempotence g v
          quickCheck $ propSoundness semEqDouble g v
   f integerView
   f rationalView
   f integerNormalForm
   f rationalNormalForm
   f rationalRelaxedForm
   
   putStrLn "*** Normal forms"
   let f v = flip mapM_ numGenerators $ \g -> do
          quickCheck $ propNormalForm g v
   f integerNormalForm
   f rationalNormalForm

   putStrLn "*** Correctness generators"
   let f g v = quickCheck $ forAll (sized g) $ \a -> a `belongsTo` v
   f integerGenerator integerView
   f rationalGenerator rationalView
   f ratioExprGen rationalNormalForm
   f ratioExprGenNonZero rationalNormalForm
   
   putStrLn "*** View relations"
   let va .>. vb = flip mapM_ numGenerators $ \g -> 
          quickCheck $ forAll g $ \a -> 
             not (a `belongsTo` va) || a `belongsTo` vb
   integerNormalForm .>. integerView
   rationalNormalForm .>. rationalRelaxedForm
   rationalRelaxedForm .>. rationalView
   integerNormalForm .>. rationalNormalForm
   integerView .>. rationalView
   
   putStrLn "*** Pre/post conditions strategies"
   let f s pre post = flip mapM_ numGenerators $ \g -> 
          quickCheck $ forAll g $ \a ->
             not (a `belongsTo` pre) || applyD s a `belongsTo` post
   f naturalStrategy  integerView  integerNormalForm
   f integerStrategy  integerView  integerNormalForm
   f rationalStrategy rationalView rationalNormalForm
   f fractionStrategy rationalView rationalNormalForm
   
numGenerators :: [Gen Expr]
numGenerators = map sized 
   [ integerGenerator, rationalGenerator
   , ratioExprGen, ratioExprGenNonZero, numGenerator
   ]
   
semEqDouble :: Expr -> Expr -> Bool
semEqDouble a b = 
   case (match doubleView a, match doubleView b) of
      (Just a, Just b)   -> a ~= b
      (Nothing, Nothing) -> True
      _                  -> False
 where
   delta = 0.0001
 
   (~=) :: Double -> Double -> Bool
   a ~= b | abs a < delta || abs b < delta = True
          | otherwise = abs (1 - (a/b)) < delta