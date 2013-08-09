module Ideas.Common.Strategy.Sequential 
   ( Sequential(..)
   , Process
   , Builder, build
   , empty, firsts, scanChoice, prune
   , fromAtoms
   , Sym(..)
   , atomic, concurrent, (<@>)
   , emptyPath, Path, withPath, replay
   , independent, uniquePath, clean
   ) where

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

run :: Process a -> [[a]]
run p = 
   [ [] | empty p ] ++
   [ a:as | (a, q) <- firsts p, as <- run q ]

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

indep :: Char -> Char -> Bool
indep x y = x `elem` "abc" && y `elem` "def"

(%) :: Sequential f => Process a -> Process a -> f a
(%) = concurrent (const True)

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
   
replay :: Monad m => Path -> Process a -> m ([a], Process a)
replay = rec []
 where
   rec acc (0, []) p         = return (acc, p)
   rec acc path    (p :|: q) = choose acc path p q
   rec acc path    (p :?: q) = choose acc path p q
   rec acc (n, bs) (a :~> p)
      | n > 0                = rec (a:acc) (n-1, bs) p
   rec _  _        _         = error "replay: invalid path"
   
   choose acc (n, b:bs) p q 
      | n > 0 = rec acc (n-1, bs) (if b then p else q)
   choose _ _ _ _ = error "replay: invalid path"

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
independent f p0 = rec p0
    where
      xs = map fst (firsts p0)     
      rec (p :|: q) = rec p <|> rec q
      rec (p :?: q) = rec p :?: rec q
      rec (a :~> p) = a :~> undefined -- independent f (filterP (\y -> all ((==) y) [ x | x <- xs, f x a ]) p)
      rec Ok        = Ok
      rec Stop      = Stop


-- Alex' stuff -----------------------------------------------------------------

-- translating the interleave combinator to a process introduces many choices with
-- a stop (which is the unit of a choice)
clean :: Process a -> Process a 
clean = fold Alg
   { forChoice = \x y -> 
                 case (x, y) of
                   (Stop, _) -> y
                   (_, Stop) -> x 
                   _         -> x <|> y
   , forEither = (<?>)
   , forPrefix = (~>)
   , forOk     = ok
   , forStop   = stop
   }

-- The uniquePath transformation changes the process in such a way that all 
-- intermediate states can only be reached by one path. A prerequisite is that
-- symbols are unique (or only used once).
uniquePath :: (a -> Bool) -> (a -> a -> Bool) -> Process a -> Process a
uniquePath pred eq = rec
    where
      rec (p :|: q) = let f x = not $ any (eq x) (map fst $ firstsWith pred p)
                      in  rec p <|> rec (filterP f q)
      rec (p :?: q) = rec p :?: rec q
      rec (a :~> p) = a :~> rec p
      rec Ok        = Ok
      rec Stop      = Stop

firstsWith :: (a -> Bool) -> Process a -> [(a, Process a)]
firstsWith p = concatMap f . firsts
  where
    f (r, q) | p r       = [(r, q)]
             | otherwise = firstsWith p q

isMajor :: String -> Bool
isMajor r = r /= "Enter" && r /= "Exit" 

-- derived from a <%> b
sp :: Process String
sp = 
  stop <|> (("Enter" ~> ("a" ~> stop <|> (("Enter" ~> ("b" ~> stop <|> (("Exit" ~> stop <|> (("Exit" ~> ok <|> (stop <|> stop)) <|> stop)) <|> ("Exit" ~> stop <|> (("Exit" ~> ok <|> (stop <|> stop)) <|> stop))))) <|> ("Exit" ~> stop <|> (("Enter" ~> ("b" ~> stop <|> (stop <|> ("Exit" ~> ok <|> (stop <|> stop))))) <|> stop))))) <|> ("Enter" ~> ("b" ~> stop <|> (("Enter" ~> ("a" ~> stop <|> (("Exit" ~> stop <|> (("Exit" ~> ok <|> (stop <|> stop)) <|> stop)) <|> ("Exit" ~> stop <|> (("Exit" ~> ok <|> (stop <|> stop)) <|> stop))))) <|> ("Exit" ~> stop <|> (("Enter" ~> ("a" ~> stop <|> (stop <|> ("Exit" ~> ok <|> (stop <|> stop))))) <|> stop))))))

-- cleaned_sp = clean sp
cleaned_sp = 
    ("Enter" :~> ("a" :~> ("Enter" :~> ("b" :~> ("Exit" :~> ("Exit" :~> Ok)) 
                                                :|: 
                                                ("Exit" :~> ("Exit" :~> Ok)))) 
                          :|: 
                          ("Exit" :~> ("Enter" :~> ("b" :~> ("Exit" :~> Ok)))))) 
    :|: 
    ("Enter" :~> ("b" :~> ("Enter" :~> ("a" :~> ("Exit" :~> ("Exit" :~> Ok)) 
                                                :|: 
                                                ("Exit" :~> ("Exit" :~> Ok)))) 
                          :|: 
                          ("Exit" :~> ("Enter" :~> ("a" :~> ("Exit" :~> Ok))))))

{-
-- filtered_sp = unique_path cleaned_sp
filtered_sp = 
    ("Enter" :~> ("a" :~> ("Enter" :~> ("b" :~> ("Exit" :~> ("Exit" :~> Ok)) 
                                                :|: 
                                                ("Exit" :~> ("Exit" :~> Ok)))) 
                          :|: 
                          ("Exit" :~> ("Enter" :~> Ok)))) 
    :|: 
    ("Enter" :~> ("b" :~> ("Enter" :~> Ok) 
                          :|: 
                          ("Exit" :~> ("Enter" :~> Ok))))


csp2 = 
    (Enter :~> (a :~> (Enter :~> (c :~> (Exit :~> (Exit :~> (Enter :~> (b :~> (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) 
                                                                                                :|: 
                                                                                                (Exit :~> (Exit :~> Ok)))) 
                                                                              :|: 
                                                                              (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))))) 
                                                            :|: 
                                                            (Enter :~> (d :~> (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) 
                                                                                                :|: 
                                                                                                (Exit :~> (Exit :~> Ok)))) 
                                                                              :|: 
                                                                              (Exit :~> (Enter :~> (b :~> (Exit :~> Ok))))))) 
                                                   :|: 
                                                   (Enter :~> (b :~> (Exit :~> (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))) 
                                                                               :|: 
                                                                               (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) 
                                                                                                 :|: 
                                                                                                 (Exit :~> (Exit :~> Ok))))) 
                                                                     :|: 
                                                                     (Exit :~> (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))))))) 
                                        :|: 
                                        (Exit :~> (Exit :~> (Enter :~> (d :~> (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) 
                                                                                                :|: 
                                                                                                (Exit :~> (Exit :~> Ok)))) 
                                                                              :|: 
                                                                              (Exit :~> (Enter :~> (b :~> (Exit :~> Ok)))))) 
                                                            :|: 
                                                            (Enter :~> (b :~> (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) 
                                                                                                :|: 
                                                                                                (Exit :~> (Exit :~> Ok)))) 
                                                                              :|: 
                                                                              (Exit :~> (Enter :~> (d :~> (Exit :~> Ok))))))) 
                                                  :|: 
                                                  (Enter :~> (d :~> (Exit :~> (Exit :~> (Enter :~> (b :~> (Exit :~> Ok)))) 
                                                                              :|: 
                                                                              (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) 
                                                                                                :|: 
                                                                                                (Exit :~> (Exit :~> Ok))))) 
                                                                    :|: 
                                                                    (Exit :~> (Exit :~> (Enter :~> (b :~> (Exit :~> Ok)))))))))) 
                :|: 
                (Exit :~> (Enter :~> (c :~> (Enter :~> (b :~> (Exit :~> (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))) :|: (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok))))) :|: (Exit :~> (Exit :~> (Enter :~> (d :~> (Exit :~> Ok))))))) :|: (Exit :~> (Enter :~> (b :~> (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok)))) :|: (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))))) :|: (Enter :~> (d :~> (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok)))) :|: (Exit :~> (Enter :~> (b :~> (Exit :~> Ok))))))))) :|: (Enter :~> (b :~> (Enter :~> (c :~> (Exit :~> (Exit :~> (Enter :~> (d :~> (Exit :~> Ok))))) :|: (Exit :~> (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))) :|: (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok))))))) :|: (Exit :~> (Enter :~> (c :~> (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))))))))))) :|: (Enter :~> (c :~> (Enter :~> (a :~> (Exit :~> (Exit :~> (Enter :~> (d :~> (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok)))) :|: (Exit :~> (Enter :~> (b :~> (Exit :~> Ok)))))) :|: (Enter :~> (b :~> (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok)))) :|: (Exit :~> (Enter :~> (d :~> (Exit :~> Ok))))))) :|: (Enter :~> (d :~> (Exit :~> (Exit :~> (Enter :~> (b :~> (Exit :~> Ok)))) :|: (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok))))) :|: (Exit :~> (Exit :~> (Enter :~> (b :~> (Exit :~> Ok)))))))) :|: (Exit :~> (Exit :~> (Enter :~> (b :~> (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok)))) :|: (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))))) :|: (Enter :~> (d :~> (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok)))) :|: (Exit :~> (Enter :~> (b :~> (Exit :~> Ok))))))) :|: (Enter :~> (b :~> (Exit :~> (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))) :|: (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok))))) :|: (Exit :~> (Exit :~> (Enter :~> (d :~> (Exit :~> Ok)))))))))) :|: (Exit :~> (Enter :~> (a :~> (Enter :~> (d :~> (Exit :~> (Exit :~> (Enter :~> (b :~> (Exit :~> Ok)))) :|: (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok))))) :|: (Exit :~> (Exit :~> (Enter :~> (b :~> (Exit :~> Ok))))))) :|: (Exit :~> (Enter :~> (d :~> (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok)))) :|: (Exit :~> (Enter :~> (b :~> (Exit :~> Ok)))))) :|: (Enter :~> (b :~> (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok)))) :|: (Exit :~> (Enter :~> (d :~> (Exit :~> Ok))))))))) :|: (Enter :~> (d :~> (Enter :~> (a :~> (Exit :~> (Exit :~> (Enter :~> (b :~> (Exit :~> Ok))))) :|: (Exit :~> (Exit :~> (Enter :~> (b :~> (Exit :~> Ok)))) :|: (Enter :~> (b :~> (Exit :~> (Exit :~> Ok)) :|: (Exit :~> (Exit :~> Ok))))))) :|: (Exit :~> (Enter :~> (a :~> (Exit :~> (Enter :~> (b :~> (Exit :~> Ok))))))))))))

fsp2 = 
    (Enter :~> (a :~> (Enter :~> (c :~> (Exit :~> (Exit :~> (Enter :~> (b :~> (Enter :~> (d :~> (Exit :~> (Exit :~> Ok)) 
                                                                                                :|: (Exit :~> (Exit :~> Ok)))) 
                                                                              :|: 
                                                                              (Exit :~> (Enter :~> Stop)))) 
                                                            :|: 
                                                            (Enter :~> (d :~> (Enter :~> Stop) 
                                                                              :|: 
                                                                              (Exit :~> (Enter :~> Stop))))) 
                                                   :|: 
                                                   (Enter :~> Stop)) 
                                        :|: 
                                        (Exit :~> (Exit :~> (Enter :~> Stop) 
                                                            :|: 
                                                            (Enter :~> Stop)) 
                                                  :|: 
                                                  (Enter :~> Stop)))) 
                      :|: 
                      (Exit :~> (Enter :~> Stop) 
                                :|: 
                                (Enter :~> (b :~> (Enter :~> Stop) 
                                                  :|: 
                                                  (Exit :~> (Enter :~> Stop))))))) 
    :|: 
    (Enter :~> (c :~> (Enter :~> Stop) 
                      :|: 
                      (Exit :~> (Enter :~> Stop) 
                                :|: 
                                (Enter :~> (d :~> (Enter :~> Stop) 
                                                  :|: 
                                                  (Exit :~> (Enter :~> Stop)))))))
-}
