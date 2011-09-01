-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Domain.Math.Power.Views
   ( -- * Power views
     -- ** Simple power views
     powerView, powerViewWith, powerViewFor, powerFactorView
     -- ** Views for power expressions with a constant factor
   , consPowerView
     -- ** Power views that allow constants
   , unitPowerView, unitPowerViewVar, strictPowerView
     -- Root views
   , rootView, strictRootView
     -- * Log view
   , logView
     -- * Other views
   , plainNatView, plainRationalView
   ) where

import Common.Library hiding (root)
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Power.Utils

-- Power views with constant factor -----------------------------------------

consPowerView :: View Expr (Expr, (Expr, Expr))
consPowerView = makeView f g
 where
   f (Negate a) = fmap (first Negate) (f a)
   f (a :*: b)  = fmap ((,) a) (match powerView b)
   f expr       = f (1 :*: expr)
   g = build (timesView >>> second powerView)

unitPowerViewWith :: View Expr a -> View Expr (Expr, (a, Expr))
unitPowerViewWith v = makeView f g
 where
   mv = powerViewWith v identity
   f (Negate a) = fmap (first Negate) (f a)
   f (a :*: b)  = do
         x <- match mv b
         return (a, x)
       `mplus` do
         x <- match v b
         return (a, (x, 1))
   f expr = f (1 :*: expr)
   g = build (timesView >>> second mv)

unitPowerViewVar :: View Expr (Expr, (String, Expr))
unitPowerViewVar = unitPowerViewWith variableView

-- | Careful! This view will match anything, so use it wise and with care.
unitPowerView :: View Expr (Expr, (Expr, Expr))
unitPowerView = unitPowerViewWith identity

-- | A root view
rootView :: View Expr (Expr, Expr)
rootView = makeView f (uncurry root)
  where
    f expr = do
      (a, (x, y)) <- match (powerView >>> second divView) expr
      guard (x `elem` [1, -1])
      return $ if x == 1 then (a, y) else (a, negate y)

-- | only matches sqrt and root
strictRootView :: View Expr (Expr, Expr)
strictRootView = makeView f g
  where
    f expr =
      case expr of
        Sym s [a, b] | isRootSymbol s -> return (a, b)
        Sqrt e                       -> return (e, 2)
        _ -> Nothing

    g (a, b) = if b == 2 then Sqrt a else root a b

-- Power views --------------------------------------------------------------

strictPowerView :: View Expr (Expr, Expr)
strictPowerView = makeView f (uncurry (.^.))
  where
    f expr =
      case expr of
        Sym s [a, b] | isPowerSymbol s -> return (a, b)
        _ -> Nothing

powerView :: View Expr (Expr, Expr)
powerView = matcherView f g
  where
    f = matcher (strictRootView >>> second (arr (1 ./.)))
        <+> matcher strictPowerView
    g (a, b) =
       case b of
         (Nat 1 :/: b') -> build strictRootView (a, b')
         _              -> build strictPowerView (a, b)

powerViewWith :: View Expr a -> View Expr b -> View Expr (a, b)
powerViewWith va vb = powerView >>> (va *** vb)

powerViewForWith :: Eq a => View Expr a -> View Expr b -> a -> View Expr b
powerViewForWith va vb a = makeView f ((build va a .^.) .  build vb)
  where
    f expr = do
      (a', b) <- match (powerViewWith va vb) expr
      guard $ a == a'
      return b

powerViewFor :: Expr -> View Expr Expr
powerViewFor = powerViewForWith identity identity

powerFactorView :: (Expr -> Expr -> Bool) -> Isomorphism Expr (Bool, [Expr])
powerFactorView p = productView >>> second (f <-> id)
  where
    f = map (build productView . (,) False) . joinBy p

-- Log views ----------------------------------------------------------------

logView :: View Expr (Expr, Expr)
logView = makeView f (uncurry logBase)
  where
    f expr = case expr of
        Sym s [a, b] | isLogSymbol s -> return (a, b)
        _ -> Nothing