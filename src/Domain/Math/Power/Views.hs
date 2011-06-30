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
-----------------------------------------------------------------------------

module Domain.Math.Power.Views
   ( -- * Power views
   
     -- ** Simple power views
     powerView, powerViewWith, powerViewForWith, powerViewFor, powerFactorView

     -- ** Views for power expressions with a constant factor
   , consPowerView, consPowerViewForWith, consPowerViewFor,consPowerViewForVar

     -- ** Power views that allow constants
   , unitPowerViewForVar, unitPowerViewVar, unitPowerView, strictPowerView

     -- Root views
   , rootView, strictRootView

     -- * Log view
   , logView

     -- * Other views
   , plainNatView, plainRationalView, varView
   ) where

import Control.Monad
import Common.Library hiding (root)
import Domain.Math.Expr
import Domain.Math.Power.Utils

-- Power views with constant factor -----------------------------------------

consPowerView :: View Expr (Expr, (Expr, Expr))
consPowerView = addNegativeView $ addUnitTimesView powerView

consPowerViewForWith :: Num a => View Expr a -> View Expr b -> a -> View Expr (Expr, b)
consPowerViewForWith va vb a = 
  addNegativeView $ addUnitTimesView (powerViewForWith va vb a)

consPowerViewFor :: Expr -> View Expr (Expr, Expr)
consPowerViewFor = consPowerViewForWith identity identity

consPowerViewForVar :: String -> View Expr (Expr, Expr)
consPowerViewForVar = consPowerViewFor . Var

unitPowerViewForVar :: String -> View Expr (Expr, Expr)
unitPowerViewForVar s = makeView f g
  where
    f expr = do
      (c, (s', x)) <- match unitPowerViewVar expr
      guard $ s == s'
      return (c, x)
    g (c, x) = build unitPowerViewVar (c , (s, x))

unitPowerViewWith :: View Expr a -> View Expr (Expr, (a, Expr))
unitPowerViewWith v = addNegativeView $ addUnitTimesView $ 
  powerViewWith v identity <&> (unitTimes v >>> toView swapView)

unitPowerViewVar :: View Expr (Expr, (String, Expr))
unitPowerViewVar = unitPowerViewWith varView

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
powerView = makeView f g 
  where
    f = match ((strictRootView >>> second ((1 ./.) <-> id)) <&> strictPowerView)
    g (a, b) = 
       case b of 
         (Nat 1 :/: b') -> build strictRootView (a, b')
         _              -> build strictPowerView (a, b)

powerViewWith :: View Expr a -> View Expr b -> View Expr (a, b)
powerViewWith va vb = powerView >>> first va >>> second vb

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


-- Help (non-power) views ---------------------------------------------------

unitTimes :: Num t => View a b -> View a (t, b)
unitTimes v = v >>> ((,) 1 <-> snd)

addTimesView :: View Expr a -> View Expr (Expr, a)
addTimesView v = timesView >>> second v

addUnitTimesView :: View Expr a -> View Expr (Expr, a)
addUnitTimesView v = addTimesView v <&> unitTimes v

negateView :: (Num a, WithFunctions a) => View a a
negateView = makeView isNegate negate

addNegativeView :: View Expr a -> View Expr a
addNegativeView v = v <&> (negateView >>> v)

varView :: View Expr String
varView = makeView f Var
  where
    f (Var s) = Just s
    f _       = Nothing
