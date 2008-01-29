module Common.RegExp
   ( RegExp, (<*>), (<|>), star, fix, succeed, fail, symbol
   , collectSymbols, join, isSucceed, firsts, nonSucceed
   , language, member, checks, withIndex
   ) where

import Prelude hiding (fail)
import Common.Utils
import Control.Monad (liftM, liftM2)
import Control.Arrow (second)
import Test.QuickCheck 

infixr 5 <*>
infixr 4 <|>

data RegExp a
   = RegExp a :*: RegExp a
   | RegExp a :|: RegExp a
   | Fix Int (RegExp a)
   | Var Int
   | Symbol a
   | Succeed 
   | Fail
 deriving Show

instance Functor RegExp where
   fmap f regexp =
      case regexp of
         p :*: q  -> fmap f p :*: fmap f q
         p :|: q  -> fmap f p :|: fmap f q
         Fix i p  -> Fix i (fmap f p) 
         Var i    -> Var i
         Symbol a -> Symbol (f a)
         Succeed  -> Succeed
         Fail     -> Fail

join :: RegExp (RegExp a) -> RegExp a
join regexp =
   case regexp of
      p :*: q  -> join p <*> join q
      p :|: q  -> join p <|> join q
      Fix i p  -> Fix i (join p)
      Var i    -> Var i
      Symbol a -> a
      Succeed  -> succeed
      Fail     -> fail

-- smart constructor
(<*>) :: RegExp a -> RegExp a -> RegExp a
p <*> q = 
   case (p, q) of
      (Fail, _)    -> Fail
      (_, Fail)    -> Fail
      (Succeed, _) -> q
      (_, Succeed) -> p
      _            -> p :*: q

-- smart constructor
(<|>) :: RegExp a -> RegExp a -> RegExp a
p <|> q = 
   case (p, q) of
      (Fail, _) -> q
      (_, Fail) -> p
      _ | isSucceed p || isSucceed q -> 
             case nonSucceed p <|> nonSucceed q of 
                Fail -> Succeed
                new  -> Succeed :|: new
        | otherwise -> p :|: q

fix :: (RegExp a -> RegExp a) -> RegExp a
fix f = 
   let p = f (Var i)
       i = maximum (-1 : getFixVars p) + 1
   in Fix i p

symbol :: a -> RegExp a
symbol = Symbol
         
succeed :: RegExp a
succeed  = Succeed

fail :: RegExp a
fail = Fail

-- little cheat: the nonSucceed part
star :: RegExp a -> RegExp a
star p = fix $ \this -> (nonSucceed p <*> this) <|> succeed

getFixVars :: RegExp a -> [Int]
getFixVars regexp =
   case regexp of
      p :*: q  -> getFixVars p ++ getFixVars q
      p :|: q  -> getFixVars p ++ getFixVars q
      Fix i p  -> i : getFixVars p
      _        -> []

replaceFixVar :: Int -> RegExp a -> RegExp a -> RegExp a
replaceFixVar i new = rec
 where
   rec regexp =
      case regexp of
         p :*: q  -> rec p <*> rec q
         p :|: q  -> rec p <|> rec q
         Fix j p | i /= j -> Fix j (rec p)
         Var j   | i == j -> new
         _        -> regexp
      
unfoldFix :: Int -> RegExp a -> RegExp a
unfoldFix i p = replaceFixVar i (Fix i p) p

collectSymbols :: RegExp a -> [a]
collectSymbols regexp = 
   case regexp of
      p :*: q  -> collectSymbols p ++ collectSymbols q
      p :|: q  -> collectSymbols p ++ collectSymbols q
      Fix i p  -> collectSymbols p
      Symbol a -> [a]
      _        -> []
  
isSucceed :: RegExp a -> Bool
isSucceed regexp =
   case regexp of
      p :*: q -> isSucceed p && isSucceed q
      p :|: q -> isSucceed p || isSucceed q
      Fix i p -> isSucceed p
      Succeed -> True
      _ -> False

-- regular expression without the Succeed alternative
nonSucceed :: RegExp a -> RegExp a
nonSucceed regexp =
   case regexp of
      p :*: q |  isSucceed p && isSucceed q 
              -> let nsp = nonSucceed p
                     nsq = nonSucceed q
                 in nsp <|> nsq <|> (nsp <*> nsq)
      p :|: q -> nonSucceed p <|> nonSucceed q
      Fix i p -> nonSucceed (unfoldFix i p)
      Succeed -> fail
      _       -> regexp

firsts :: RegExp a -> [(a, RegExp a)]
firsts regexp =
   case regexp of
      Symbol r -> [(r, succeed)]
      p :|: q  -> firsts p ++ firsts q
      p :*: q  -> map (second (<*> q)) (firsts p) ++ if isSucceed p then firsts q else []
      Fix i p  -> firsts (unfoldFix i p)
      _        -> []

withIndex :: RegExp a -> RegExp (Int, a)
withIndex = snd . rec 0
 where
   rec n regexp =
      case regexp of  
         p :*: q  -> let (n1, a) = rec n  p
                         (n2, b) = rec n1 q
                     in (n2, a :*: b)
         p :|: q  -> let (n1, a) = rec n  p
                         (n2, b) = rec n1 q
                     in (n2, a :|: b)
         Fix i p  -> let (n1, a) = rec n p
                     in (n1, Fix i a)
         Var i    -> (n, Var i)
         Symbol a -> (n+1, Symbol (n, a))
         Succeed  -> (n, Succeed)
         Fail     -> (n, Fail)
                       
language :: RegExp a -> [[a]]
language regexp = 
   case regexp of
      p :*: q  -> [ xs ++ ys | xs <- language p, ys <- language q ]
      p :|: q  -> language p ++ language q
      Fix i p  -> language (unfoldFix i p)
      Symbol a -> [[a]]
      Succeed  -> [[]]
      Fail     -> []

member :: Eq a => RegExp a -> [a] -> Bool
member regexp = not . null . filter null . rec regexp
 where
   rec regexp xs =
      case regexp of
         p :*: q  -> [ zs | ys <- rec p xs, zs <- rec q ys ]
         p :|: q  -> rec p xs ++ rec q xs
         Fix i p  -> rec (unfoldFix i p) xs
         Symbol a -> case xs of
                        y:ys | y==a -> [ys]
                        _           -> []
         Succeed  -> [xs]
         Fail     -> []
      
--------------------------------------------------------
-- QuickCheck generator

instance Arbitrary a => Arbitrary (RegExp a) where
   arbitrary = sized arbAnnRegExp
   coarbitrary regexp =
      case regexp of
         p :*: q  -> variant 0 . coarbitrary p . coarbitrary q
         p :|: q  -> variant 1 . coarbitrary p . coarbitrary q
         Fix i p  -> variant 2 . coarbitrary i . coarbitrary p
         Var i    -> variant 3 . coarbitrary i
         Symbol a -> variant 4 . coarbitrary a
         Succeed  -> variant 5
         Fail     -> variant 6

-- Use smart constructors here
arbAnnRegExp :: Arbitrary a => Int -> Gen (RegExp a)
arbAnnRegExp 0 = oneof [ liftM symbol arbitrary, return succeed, return fail ]
arbAnnRegExp n = oneof [ liftM2 (<*>) rec rec, liftM2 (<|>) rec rec
                       , arbAnnRegExp 0, liftM star rec -- , liftM fix (bla (n `div` 2))
                       ]
 where rec = arbAnnRegExp (n `div` 2)

bla :: Arbitrary a => Int -> Gen (RegExp a -> RegExp a)
bla n = promote f
 where f re = coarbitrary re (arbAnnRegExp n)

--------------------------------------------------------
-- QuickCheck properties                                                                 

propSound :: RegExp Int -> Property
propSound p = not (null xs) ==> {- collect (length xs) $ -} all (member p) xs
 where xs = take 20 $ language p

propNonSucceed :: RegExp Int -> Bool
propNonSucceed p = not $ member (nonSucceed p) []

propSplitSucceed :: RegExp Int -> Bool
propSplitSucceed p = p === if isSucceed p then succeed <|> new else new
 where new = nonSucceed p

propFirsts :: RegExp Int -> Bool
propFirsts p = nonSucceed p === foldr op fail (firsts p)
 where op (a, q) r = (symbol a <*> q) <|> r

propJoin :: RegExp Int -> Bool
propJoin p = join (fmap symbol p) === p
          
propMap :: (Int -> Int) -> (Int -> Int) -> RegExp Int -> Bool
propMap f g p = fmap f (fmap g p) === fmap (f . g) p

propFix :: RegExp Int -> Property
propFix this@(Fix i p) = property (unfoldFix i p === this)
propFix _              = False ==> True

propSucceed :: RegExp Int -> Bool
propSucceed p = isSucceed p == member p []

infixl 1 ===
 
(===) :: RegExp Int -> RegExp Int -> Bool
p === q = all (member p) ys && all (member q) xs 
 where
   xs = take 20 $ language p
   ys = take 20 $ language q
   
reflexive p           =  p==p
associative op p q r  =  p `op` (q `op` r) === (p `op` q) `op` r
commutative op p q    =  p `op` q === q `op` p
idempotent  op p      =  p `op` p === p
leftUnit    op e p    =  e `op` p === p
rightUnit   op e p    =  p `op` e === p
unit        op e p    =  leftUnit op e p && rightUnit op e p
propStar         p    =  star p === succeed <|> (p <*> star p)

checks :: IO ()
checks = do
   -- let thoroughCheck p = verboseCheck p
   thoroughCheck propSound
   thoroughCheck propNonSucceed
   thoroughCheck propSplitSucceed
   thoroughCheck propFirsts
   quickCheck propMap
   quickCheck propJoin
   quickCheck propFix
   quickCheck propStar
   quickCheck propSucceed
   quickCheck $ associative (<|>)
   quickCheck $ commutative (<|>)
   quickCheck $ idempotent  (<|>)
   quickCheck $ unit (<|>) fail
   quickCheck $ associative (<*>)
   quickCheck $ unit (<*>) succeed