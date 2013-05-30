module Domain.Math.Fraction.Rules where

import Common.Library
import Domain.Math.Expr.Clipboard
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Numeric.Rules (calcPlusWith, calcMinusWith, calcTimesWith, calcDivisionWith)

expandFractionSymbol :: Symbol
expandFractionSymbol = newSymbol "elementary.expand_fraction"

reduceFractionSymbol :: Symbol
reduceFractionSymbol = newSymbol "elementary.reduce_fraction"



-- Matching, borrowing from "Canonical forms..." MKM
additionView :: View Expr (Expr, Expr)
additionView = makeView f g 
 where
   f (a :+: b) = Just (a,b)
   f _         = Nothing

   g (a, b) = a :+: b

fractionView :: View Expr (Expr, Expr)
fractionView = makeView f g 
 where
   f (a :/: b) = Just (a,b)
   f _         = Nothing

   g (a, b) = a :/: b


-- Find LCM, store it in the context
findLCM :: Rule (Context Expr)
findLCM = makeRule "findLCM" $ \ ctx -> do
            expr <- fromContext ctx
            (e1,e2) <- match additionView expr
            (Nat _,Nat b)   <- match fractionView e1
            (Nat _,Nat d)   <- match fractionView e2
            guard (b/=d)
            return $ addToClipboard "lcm" (Nat (lcm b d)) ctx
            

-- expand unlike fractions to lcm if necessary
expandToLCM :: Rule (Context Expr)
expandToLCM = makeRule "expandToLCM" $ \ctx -> do
                 expr <- fromContext ctx 
                 (Nat a,Nat b) <- match fractionView expr
                 lcm <- lookupClipboardG "lcm" ctx
                 guard (b /= lcm && lcm `mod` b == 0)
                 return $ replaceInContext (Nat(a * lcm `div` b) :/: Nat lcm) ctx

addLikeFractions :: Rule Expr 
addLikeFractions = makeRule "addLikeFractions" f 
 where f ((Nat a :/: Nat b) :+: (Nat c :/: Nat d)) | b==d = 
         Just (Nat (a+c) :/: Nat(b))
       f _  = Nothing 


-- Extra rules for diagnostics

gcdRule :: Rule Expr
gcdRule = makeRule "gcd" f
  where
    f (Sym gs [Nat a , Nat b]) | gs == gcdSymbol =
      Just (Nat (gcd a b))
    f _ = Nothing  

lcmRule :: Rule Expr
lcmRule = makeRule  "lcm" f
  where
    f (Sym ls [Nat a, Nat b]) | ls == lcmSymbol =
      Just (Nat (lcm a b))
    f _ = Nothing


expandRule :: Rule Expr
expandRule = makeRule "expand" f
 where
   f (Sym efs [Nat a :/: Nat b, Nat c]) | efs == expandFractionSymbol =
      Just (Nat (a*c) :/:Nat (b*c))
   f _ = Nothing

reduceRule :: Rule Expr
reduceRule = makeRule "reduce" f
  where
    f (Sym cfs [Nat a :/: Nat b, Nat c]) | a `mod` c == 0 && b `mod`c == 0 && cfs == reduceFractionSymbol =
      Just (Nat (a `div` c) :/: Nat (b `div` c))
    f _ = Nothing