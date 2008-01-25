module Common.RegExp 
   ( RegExp, (<*>), (<|>), star, succeed, fail, symbol
   , collectSymbols, join, isSucceed, isFail, firsts, nonSucceed
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
   | Star (RegExp a)
   | Symbol a
   | Succeed 
   | Fail
 deriving Show
 
instance Functor RegExp where
   fmap f regexp =
      case regexp of
         p :*: q  -> fmap f p :*: fmap f q
         p :|: q  -> fmap f p :|: fmap f q
         Star p   -> Star (fmap f p)
         Symbol a -> Symbol (f a)
         Succeed  -> Succeed
         Fail     -> Fail

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

-- smart constructor             
star :: RegExp a -> RegExp a
star p = 
   case p of
      Star _  -> p
      Succeed -> p
      Fail    -> succeed 
      _       -> Star (nonSucceed p)

symbol :: a -> RegExp a
symbol = Symbol
         
succeed :: RegExp a
succeed  = Succeed

fail :: RegExp a
fail = Fail
     
collectSymbols :: RegExp a -> [a]
collectSymbols regexp = 
   case regexp of
      p :*: q  -> collectSymbols p ++ collectSymbols q
      p :|: q  -> collectSymbols p ++ collectSymbols q
      Star p   -> collectSymbols p
      Symbol a -> [a]
      _        -> []
                 
isSucceed :: RegExp a -> Bool
isSucceed regexp =
   case regexp of
      p :*: q -> isSucceed p && isSucceed q
      p :|: q -> isSucceed p || isSucceed q
      Star p  -> True
      Succeed -> True
      _ -> False

isFail :: RegExp a -> Bool
isFail Fail = True
isFail _    = False

-- regular expression without the Succeed alternative
nonSucceed :: RegExp a -> RegExp a
nonSucceed regexp =
   case regexp of
      p :*: q |  isSucceed p && isSucceed q 
              -> let nsp = nonSucceed p
                     nsq = nonSucceed q
                 in nsp <|> nsq <|> (nsp <*> nsq)
      p :|: q -> nonSucceed p <|> nonSucceed q
      Star p  -> p <*> Star p
      Succeed -> fail
      _       -> regexp

firsts :: RegExp a -> [(a, RegExp a)]
firsts regexp =
   case regexp of
      Symbol r -> [(r, succeed)]
      p :|: q  -> firsts p ++ firsts q
      p :*: q  -> map (second (<*> q)) (firsts p) ++ if isSucceed p then firsts q else []
      Star p   -> map (second (<*> regexp)) (firsts p)
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
         Star p   -> let (n1, a) = rec n p
                     in (n1, Star a)
         Symbol a -> (n+1, Symbol (n, a))
         Succeed  -> (n, Succeed)
         Fail     -> (n, Fail)
                          
language :: RegExp a -> [[a]]
language regexp = 
   case regexp of
      p :*: q  -> [ xs ++ ys | xs <- language p, ys <- language q ]
      p :|: q  -> language p ++ language q
      Star p   -> [] : language (p <*> regexp)
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
         Star p   -> xs : rec (p <*> regexp) xs
         Symbol a -> case xs of
                        y:ys | y==a -> [ys]
                        _           -> []
         Succeed  -> [xs]
         Fail     -> []

join :: RegExp (RegExp a) -> RegExp a
join regexp =
   case regexp of
      p :*: q  -> join p <*> join q
      p :|: q  -> join p <|> join q
      Star p   -> star (join p)
      Symbol a -> a
      Succeed  -> succeed
      Fail     -> fail
          
--------------------------------------------------------
-- QuickCheck generator

instance Arbitrary a => Arbitrary (RegExp a) where
   arbitrary = sized arbAnnRegExp
   coarbitrary regexp =
      case regexp of
         p :*: q  -> variant 0 . coarbitrary p . coarbitrary q
         p :|: q  -> variant 1 . coarbitrary p . coarbitrary q
         Star p   -> variant 2 . coarbitrary p
         Symbol a -> variant 3 . coarbitrary a
         Succeed  -> variant 4
         Fail     -> variant 5
   
-- Use smart constructors here
arbAnnRegExp :: Arbitrary a => Int -> Gen (RegExp a)
arbAnnRegExp 0 = oneof [ liftM symbol arbitrary, return succeed, return fail ]
arbAnnRegExp n = oneof [ liftM2 (<*>) rec rec, liftM2 (<|>) rec rec
                       , arbAnnRegExp 0, liftM star rec
                       ]
 where rec = arbAnnRegExp (n `div` 2)
 
--------------------------------------------------------
-- QuickCheck properties                                                                 

propSound :: RegExp Int -> Property
propSound p = not (null xs) ==> collect (length xs) $ all (member p) xs
 where xs = take 20 $ language p

propNonSucceed :: RegExp Int -> Bool
propNonSucceed p = not $ member (nonSucceed p) []

propSplitSucceed :: RegExp Int -> Bool
propSplitSucceed p = p === if isSucceed p then succeed <|> new else new
 where new = nonSucceed p

propFirsts :: RegExp Int -> Bool
propFirsts p = nonSucceed p === foldr op fail (firsts p)
 where op (a, q) r = (symbol a <*> q) <|> r

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
   thoroughCheck propSound
   thoroughCheck propNonSucceed
   thoroughCheck propSplitSucceed
   thoroughCheck propFirsts
   quickCheck propStar
   quickCheck $ associative (<|>)
   quickCheck $ commutative (<|>)
   quickCheck $ idempotent  (<|>)
   quickCheck $ unit (<|>) fail
   quickCheck $ associative (<*>)
   quickCheck $ unit (<*>) succeed

{-
firstsLoc :: RegExp a -> [(a, Location)]
firstsLoc regexp = undefined
      
prop :: RegExp Int -> Bool
prop regexp = length list1 == length list2 && all check (zip list1 list2)
 where
   list1 = order (firsts regexp)
   list2 = order (firstsLoc regexp)
   order = map (first head . unzip) . groupBy eqFst . sortBy ordFst . take 5
   ordFst = \x y -> fst x `compare` fst y
   eqFst  = \x y -> fst x == fst y
   check ((a, p), (b, locs)) = a == b && (altList p === altList (map (`toLocation` regexp) locs)) 
   
   altList = foldr (<|>) Fail

toLocation :: Location -> RegExp a -> RegExp a
toLocation 0 regexp = regexp
toLocation n regexp =
   case regexp of
      p :*: q -> undefined
      p :|: q -> let i = length (collectSteps p) 
                 in 
      Star p  -> toLocation n p :*: regexp
      Step a  -> if n==1 then Succeed else Fail
      _       -> Fail
      -}
      {-
type Location = Int -}