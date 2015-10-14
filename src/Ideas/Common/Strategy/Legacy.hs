-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Legacy strategy combinators (before the Functor-Applicative-Monad proposal)
--
-----------------------------------------------------------------------------
--  $Id: Combinators.hs 8571 2015-08-27 08:23:13Z bastiaan $

module Ideas.Common.Strategy.Legacy where

import qualified Prelude
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Combinators

infixr 2 <%>, <@>
infixr 3 <|>
infixr 4 >|>
infixr 5 <*>

(<%>), (<@>), (<|>), (>|>), (<*>) :: 
   (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a

(<%>) = (.%.)
(<@>) = (.@.)
(<|>) = (.|.)
(>|>) = (./.)
(<*>) = (.*.)

alternatives :: IsStrategy f => [f a] -> Strategy a
alternatives = choice