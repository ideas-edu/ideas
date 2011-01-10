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
module Common.Rewriting.Axioms where

type Unary  a = a -> a
type Binary a = a -> a -> a

-- * Binary operator axioms

-- | @associative (=) (+)@ gives @a+(b+c) = (a+b)+c@
associative :: (a -> a -> b) -> Binary a -> a -> a -> a -> b
associative (~=) f = \a b c -> f a (f b c) ~= f (f a b) c 

-- | @commutative (=) (+)@ gives @a+b = b+a@
commutative :: (a -> a -> b) -> Binary a -> a -> a -> b
commutative (~=) f = \a b -> f a b ~= f b a

-- | @idempotent (=) (+)@ gives @a+a = a@
idempotent :: (a -> a -> b) -> Binary a -> a -> b
idempotent (~=) f = \a -> f a a ~= a

-- * Abelian Group axioms

-- | @leftIdentity (=) (+) 0@ gives @0+a = a@
leftIdentity :: (a -> a -> b) -> Binary a -> a -> a -> b
leftIdentity (~=) f e = \a -> f e a ~= a

-- | @rightIdentity (=) (+) 0@ gives @a+0 = a@
rightIdentity :: (a -> a -> b) -> Binary a -> a -> a -> b
rightIdentity (~=) f e = \a -> f a e ~= a

-- | @leftInverse (=) (+) (-) 0@ gives @(-a)+a = 0@
leftInverse :: (a -> a -> b) -> Binary a -> Unary a -> a -> a -> b
leftInverse (~=) f g e = \a -> f (g a) a ~= e

-- | @rightInverse (=) (+) (-) 0@ gives @a+(-a) = 0@
rightInverse :: (a -> a -> b) -> Binary a -> Unary a -> a -> a -> b
rightInverse (~=) f g e = \a -> f a (g a) ~= e

-- | @unaryCancel (=) (-)@ gives @-(-a) = a@
unaryCancel :: (a -> a -> b) -> Unary a -> a -> b
unaryCancel (~=) f = \a -> f (f a) ~= a

-- | @unaryIdentity (=) (-) 0@ gives @-0 = 0@
unaryIdentity :: (a -> a -> b) -> Unary a -> a -> b
unaryIdentity (~=) f e = f e ~= e

-- | @unaryDistributive (=) (+) (-)@ gives @-(a+b) = (-a)+(-b)@
unaryDistributive :: (a -> a -> b) -> Binary a -> Unary a -> a -> a -> b
unaryDistributive (~=) f g = \a b -> g (f a b) ~= f (g a) (g b)

-- | @flippedUnaryDistributive (=) (+) (-)@ gives @-(a+b) = (-b)+(-a)@
flippedUnaryDistributive :: (a -> a -> b) -> Binary a -> Unary a -> a -> a -> b
flippedUnaryDistributive (~=) f g = \a b -> g (f a b) ~= f (g b) (g a)

-- * Ring axioms

-- | @leftDistributive (=) (+) (*)@ gives @a*(b+c) = a*b + a*c@
leftDistributive :: (a -> a -> b) -> Binary a -> Binary a -> a -> a -> a -> b
leftDistributive (~=) f g = \a b c -> g a (f b c) ~= f (g a b) (g a c)

-- | @rightDistributive (=) (+) (*)@ gives @(a+b)*c = a*c + b*c@
rightDistributive :: (a -> a -> b) -> Binary a -> Binary a -> a -> a -> a -> b
rightDistributive (~=) f g = \a b c -> g (f a b) c ~= f (g a c) (g b c)

-- | @leftAbsorbing (=) (*) 0@ gives @0*a = 0@
leftAbsorbing :: (a -> a -> b) -> Binary a -> a -> a -> b
leftAbsorbing (~=) f z = \a -> f a z ~= z

-- | @rightAbsorbing (=) (*) 0@ gives @a*0 = 0@
rightAbsorbing :: (a -> a -> b) -> Binary a -> a -> a -> b
rightAbsorbing (~=) f z = \a -> f z a ~= z

-- * Other axioms

-- | @unaryIdempotent (=) (-)@ gives @-(-a) = a@
unaryIdempotent :: (a -> a -> b) -> Unary a -> a -> b
unaryIdempotent (~=) f = \a -> f (f a) ~= f a