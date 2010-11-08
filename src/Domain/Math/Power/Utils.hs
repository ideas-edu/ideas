-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- some of these help functions may have a broader scope and could be 
-- moved to other parts of the framework (eg. Common)
--
-----------------------------------------------------------------------------

module Domain.Math.Power.Utils where

import Prelude hiding (repeat, replicate)

import Common.Classes
import Common.Context
import Common.Exercise
import Common.Rewriting
import Common.Strategy hiding (not)
import Common.Transformation
import Common.View hiding (simplify)
import qualified Common.View as View
import Control.Monad
import Data.List hiding (repeat, replicate)
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Simplification
import Domain.Math.SquareRoot.Views


-- | Test functions -----------------------------------------------------------

showDerivations ex = mapM_ (putStrLn . showDerivation ex) $ examples ex

a = Var "a" ; b = Var "b" ; x = Var "x"


-- | Strategy functions -------------------------------------------------------

cleanUpStrategyRules :: IsTerm a => 
                        [Rule a] -> LabeledStrategy (Context a) -> 
                        LabeledStrategy (Context a)
cleanUpStrategyRules rs = cleanUpStrategy (applyD $ exhaustiveStrategy rs)

exhaustiveStrategy :: IsTerm a => [Rule a] -> Strategy (Context a)
--exhaustiveStrategy = replicate 50 . somewhere . alternatives . map liftToContext
exhaustiveStrategy = repeat . somewhere . alternatives . map liftToContext


-- | Rule functions -----------------------------------------------------------

smartRule :: Rule Expr -> Rule Expr
smartRule = doAfter f
  where
    f (a :*: b) = a .*. b
    f (a :/: b) = a ./. b
    f (Negate a) = neg a
    f (a :+: b) = a .+. b
    f (a :-: b) = a .-. b
    f e = e

mySimplify :: Expr -> Expr
mySimplify = smartConstructors 
           . distribution 
           . View.simplify (squareRootViewWith rationalView)
           . constantFolding

-- | View functions -----------------------------------------------------------

(<&>) :: (MonadPlus m) => ViewM m a b -> ViewM m a b -> ViewM m a b
v <&> w = makeView (\x -> match v x `mplus` match w x) (build v)

infixl 1 <&>


-- | Common functions ---------------------------------------------------------

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

split :: (Eq a) => (a -> a -> t) -> [a] -> [(t, [a])]    
split op xs = f xs
      where
        f (y:ys) | not (null ys) = [(y `op` z, xs \\ [y, z]) | z <- ys] ++ f ys 
                 | otherwise     = []
        f [] = []

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe p x = if p x then Just x else Nothing

joinBy :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
joinBy _  [] = []
joinBy eq xs = let (ys, zs) = f xs in ys : joinBy eq zs
  where
    f xs = let ys = dropUntil eq xs in (ys, xs \\ ys)

dropUntil :: (a -> a -> Bool) -> [a] -> [a]
dropUntil _ []       = []
dropUntil _ [x]      = [x]
dropUntil p (x:y:ys) | p x y     = x : dropUntil p (y:ys) 
                     | otherwise = [x]

holes :: [a] -> [(a, [a], a -> [a])]
holes xs = map (f xs) [0 .. length xs - 1] 
  where 
    f xs i = let (ys, z:zs) = splitAt i xs in (z, ys ++ zs, \x -> ys ++ x:zs)

twoNonAdjacentHoles :: [a] -> [((a, a), a -> [a])]
twoNonAdjacentHoles xs = concatMap g pairs
  where
    pairs = [(x, y) | x <- [0 .. length xs - 1], y <- [x + 1 .. length xs - 1]]
    g (x, y) = let (ys, z:zs) = splitAt x xs 
                   (ps, q:qs) = splitAt (y - x - 1) zs 
               in if null ps
                 then [ ((z, q), \z -> ys ++ z:ps ++ qs) ]
                 else [ ((z, q), \z -> ys ++ z:ps ++ qs)
                      , ((z, q), \q -> ys ++ ps ++ q:qs) ]
