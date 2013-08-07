module Ideas.Common.Strategy.Sequential 
   ( Sequential(..)
   , Process
   , Builder, build
   , empty, firsts, scanChoice, prune
   , fromAtoms
   , Sym(..)
   , atomic, concurrent, (<@>)
   , emptyPath, Path, withPath, replay
   , independent
   ) where

import Data.Monoid

-- always functor?
class Sequential f where
   ok, stop :: f a
   single   :: a -> f a
   (~>)     :: a -> f a -> f a
   (<|>)    :: f a -> f a -> f a
   (<?>)    :: f a -> f a -> f a
   (<*>)    :: f a -> f a -> f a
   choice   :: [f a] -> f a
   -- default implementation
   single a = a ~> ok
   a ~> p   = single a <*> p
   p <?> q  = p <|> q -- angelic by default
   choice   = foldr (<|>) stop

infixr 3 :~>, ~>

data Process a 
   = Process a :|: Process a   -- choice (p or q)
   | Process a :?: Process a   -- non-deterministic choice (behaves as either p or q)
   | a   :~> Process a         -- prefix (a then p)
   | Ok                        -- successful termination
   | Stop                      -- failure
   deriving (Show, Eq)

instance Sequential Process where
   ok    = Ok
   stop  = Stop
   (~>)  = (:~>)
   (<?>) = (:?:)
   (<|>) = (:|:)
   
   p <*> Ok = p
   p <*> q  = fold (Alg (<|>) (:?:) (:~>) q Stop) p

newtype Builder a = B (Process a -> Process a)

instance Sequential Builder where
   ok          = B id
   stop        = B (const Stop)
   single a    = B (a ~>)
   a ~> B f    = B ((a ~>) . f)
   B f <|> B g = B (\p -> f p <|> g p)
   B f <?> B g = B (\p -> f p <?> g p)
   B f <*> B g = B (f . g)

build :: Builder a -> Process a 
build (B f) = f Ok


{-
data Menu a = Menu { empty :: Bool, firstsSeq :: S.Seq (a, Menu a) }

firsts :: Menu a -> [(a, Menu a)]
firsts = toList . firstsSeq

instance Sequential Menu where
   ok      = Menu True  S.empty
   stop    = Menu False S.empty
   a ~> m  = Menu False (S.singleton (a, m))
   
   Menu b1 xs <|> Menu b2 ys = Menu (b1 || b2) (xs <> ys)
   
   Menu b xs <*> m 
      | b         = m <|> ys
      | otherwise = ys
    where
      ys = Menu b $ fmap (\(a, p) -> (a, p <*> m)) xs  -}

   
data Alg a b = Alg 
   { forChoice :: b -> b -> b
   , forEither :: b -> b -> b
   , forPrefix :: a -> b -> b
   , forOk     :: b
   , forStop   :: b
   }

{-
instance Monoid (Process a) where
   mempty  = stop
   mappend = (<|>) -}

--instance Functor Process where
--   fmap f = fold (Alg (:|:) (:?:) ((:~>) . f) Ok Stop)

{-
instance Monad Process where
   return  = (:~> ok)
   fail _  = Stop
   p >>= f = fold (Alg (:|:) (:?:) ((<*>) . f) Ok Stop) p -}

fold :: Alg a b -> Process a -> b
fold alg = rec 
 where
   rec (p :|: q) = forChoice alg (rec p) (rec q)
   rec (p :?: q) = forEither alg (rec p) (rec q)
   rec (a :~> p) = forPrefix alg a (rec p)
   rec Ok        = forOk alg
   rec Stop      = forStop alg
  
  {- 
join :: Process (Process a) -> Process a
join = fold (Alg (:|:) (:?:) (<*>) Ok Stop)
-}

-- angelic for non-deterministic choice
empty :: Process a -> Bool
empty = fold $ Alg (||) (||) (\_ _ -> False) True False

-- angelic for non-deterministic choice
firsts :: Process a -> [(a, Process a)]
firsts = ($ []) . rec 
 where
   rec (p :|: q) = rec p . rec q
   rec (p :?: q) = rec p . rec q
   rec (a :~> p) = ((a, p):)
   rec Ok        = id
   rec Stop      = id

scanChoice :: (a -> b -> [(a, c)]) -> a -> Process b -> Process c
scanChoice f = rec
 where
   rec a (p :|: q) = rec a p :|: rec a q
   rec a (p :?: q) = rec a p :?: rec a q
   rec a (b :~> p) = choice [ c :~> rec a2 p | (a2, c) <- f a b ]
   rec _ Ok        = Ok
   rec _ Stop      = Stop

-- remove left-biased choice
prune :: (a -> Bool) -> Process a -> Process a
prune f = fst . fold Alg 
   { forChoice = \ ~(p, b1) ~(q, b2) -> (p <|> q, b1 || b2)
   , forEither = \p q -> if snd p then p else q
   , forPrefix = \a ~(p, b) -> (a ~> p, f a || b)
   , forOk     = (ok, True)
   , forStop   = (stop, False)
   }

useFirst :: Sequential f => (a -> Process a -> f b) -> f b -> Process a -> f b
useFirst op e = rec 
 where
   rec (p :|: q) = rec p <|> rec q
   rec (p :?: q) = rec p <?> rec q
   rec (a :~> p) = op a p
   rec Ok        = e
   rec Stop      = stop



data Sym a = Single a | Composed (Process a)
   
fromAtoms :: Process (Sym a) -> Process a 
fromAtoms (Single a   :~> q) = a ~> fromAtoms q
fromAtoms (Composed p :~> q) = p <*> fromAtoms q
fromAtoms (p :|: q)          = fromAtoms p <|> fromAtoms q
fromAtoms (p :?: q)          = fromAtoms p <?> fromAtoms q
fromAtoms Ok                 = ok
fromAtoms Stop               = stop

atomic :: Sequential f => Process (Sym a) -> f (Sym a)
atomic = single . Composed . fromAtoms

concurrent :: Sequential f => (a -> Bool) -> Process a -> Process a -> f a
concurrent switch = normal
 where 
   normal p q = stepBoth q p <|> (stepRight q p <|> stepRight p q)

   stepBoth  = useFirst stop2 . useFirst stop2 ok
   stop2 _ _ = stop

   stepRight p = useFirst op stop
    where
      op a = (a ~>) . (if switch a then normal else stepRight) p

-- Alternate combinator
(<@>) :: Sequential f => Process a -> Process a -> f a
p <@> q = useFirst (\a r -> a ~> (q <@> r)) bothOk p
 where
   bothOk = useFirst (\_ _ -> stop) ok q

--------------------------------

abc, de :: Process  Char
abc = 'a' :~> 'b' :~> 'c' :~> ok
de  = 'd' :~> 'e' :~> ok

{-
go = run (concurrent undefined undefined abc de) -}

type Path = (Int, [Bool]) -- depth, choices

emptyPath :: Path
emptyPath = (0, [])

withPath :: Process a -> Process (a, Path)
withPath = rec 0 [] 
 where
   rec n bs (p :|: q) = rec (n+1) (True:bs) p :|: rec (n+1) (False:bs) q
   rec n bs (p :?: q) = rec (n+1) (True:bs) p :?: rec (n+1) (False:bs) q
   rec n bs (a :~> p) = ((a, (n+1, reverse bs)) :~> rec (n+1) bs p)
   rec n bs Ok        = Ok
   rec n bs Stop      = Stop
   
replay :: Monad m => Path -> Process a -> m (Process a)
replay (0, []) p         = return p
replay path    (p :|: q) = choose path p q
replay path    (p :?: q) = choose path p q
replay (n, bs) (a :~> p)
   | n > 0               = replay (n-1, bs) p
replay _       _         = error "replay: invalid path"

choose :: Monad m => Path -> Process a -> Process a -> m (Process a)
choose (n, b:bs) p q | n > 0 = replay (n-1, bs) (if b then p else q)
choose _ _ _ = error "replay: invalid path"

--------------------------------

filterP :: (a -> Bool) -> Process a -> Process a 
filterP p = fold Alg
   { forChoice = (<|>)
   , forEither = (<?>)
   , forPrefix = \a q -> if p a then a ~> q else stop
   , forOk     = ok
   , forStop   = stop
   }

independent :: (a -> a -> Bool) -> Process a -> Process a 
independent eq p0 = rec p0
 where
   rec (p :|: q) = rec p <|> rec q
   rec (p :?: q) = rec p :?: rec q
   rec (a :~> p) = a :~> rec (filterP (not . eq a) p)
   rec Ok        = Ok
   rec Stop      = Stop
