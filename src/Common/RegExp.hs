module Common.RegExp 
   ( RegExp(Step, Succeed, Fail), (<*>), (<|>), star 
   , collectSteps, join, isSucceed, firsts, nonSucceed
   , language, member
   , checks
   ) where

import Common.Utils
import Control.Monad hiding (join)
import Control.Arrow
import Test.QuickCheck 

data RegExp a
   = RegExp a :*: RegExp a
   | RegExp a :|: RegExp a
   | Star (RegExp a)
   | Step a
   | Succeed 
   | Fail
 deriving Show
 
instance Functor RegExp where
   fmap f regexp =
      case regexp of
         p :*: q -> fmap f p :*: fmap f q
         p :|: q -> fmap f p :|: fmap f q
         Step a  -> Step (f a)
         Succeed -> Succeed
         Fail    -> Fail

collectSteps :: RegExp a -> [a]
collectSteps regexp = 
   case regexp of
      p :*: q -> collectSteps p ++ collectSteps q
      p :|: q -> collectSteps p ++ collectSteps q
      Star p  -> collectSteps p
      Step a  -> [a]
      _       -> []
      
-- Smart constructors
(<*>) :: RegExp a -> RegExp a -> RegExp a
p <*> q = case (p, q) of
             (Fail, _)    -> Fail
             (_, Fail)    -> Fail
             (Succeed, _) -> q
             (_, Succeed) -> p
             _            -> p :*: q

(<|>) :: RegExp a -> RegExp a -> RegExp a
p <|> q = case (p, q) of
             (Fail, _)    -> q
             (_, Fail)    -> p
             _ | isSucceed p || isSucceed q -> 
                    case nonSucceed p <|> nonSucceed q of 
                       Fail -> Succeed
                       new  -> Succeed :|: new
               | otherwise -> p :|: q
        
star :: RegExp a -> RegExp a
star p = case p of
            Star _  -> p
            Succeed -> p
            Fail    -> Succeed 
            _       -> Star (nonSucceed p)
            
isSucceed :: RegExp a -> Bool
isSucceed regexp =
   case regexp of
      p :*: q -> isSucceed p && isSucceed q
      p :|: q -> isSucceed p || isSucceed q
      Star p  -> True
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
      Star p  -> p <*> Star p
      Succeed -> Fail
      _       -> regexp

firsts :: RegExp a -> [(a, RegExp a)]
firsts regexp =
   case regexp of
      Step r  -> [(r, Succeed)]
      p :|: q -> firsts p ++ firsts q
      p :*: q -> map (second (<*> q)) (firsts p) ++ if isSucceed p then firsts q else []
      Star p  -> map (second (<*> regexp)) (firsts p)
      _       -> []
              
language :: RegExp a -> [[a]]
language regexp = 
   case regexp of
      p :*: q -> [ xs ++ ys | xs <- language p, ys <- language q ]
      p :|: q -> language p ++ language q
      Star p  -> [] : language (p <*> regexp)
      Step a  -> [[a]]
      Succeed -> [[]]
      Fail    -> []
      
member :: Eq a => RegExp a -> [a] -> Bool
member regexp = not . null . filter null . rec regexp
 where
   rec regexp xs =
      case regexp of
         p :*: q -> [ zs | ys <- rec p xs, zs <- rec q ys ]
         p :|: q -> rec p xs ++ rec q xs
         Star p  -> xs : rec (p <*> regexp) xs
         Step a  -> case xs of
                       y:ys | y==a -> [ys]
                       _           -> []
         Succeed -> [xs]
         Fail    -> []

join :: RegExp (RegExp a) -> RegExp a
join regexp =
   case regexp of
      p :*: q -> join p <*> join q
      p :|: q -> join p <|> join q
      Star p  -> star (join p)
      Step a  -> a
      Succeed -> Succeed
      Fail    -> Fail
            
--------------------------------------------------------
-- QuickCheck generator

instance Arbitrary a => Arbitrary (RegExp a) where
   arbitrary = sized arbAnnRegExp
   coarbitrary regexp =
      case regexp of
         p :*: q -> variant 0 . coarbitrary p . coarbitrary q
         p :|: q -> variant 1 . coarbitrary p . coarbitrary q
         Star p  -> variant 2 . coarbitrary p
         Step a  -> variant 3 . coarbitrary a
         Succeed -> variant 4
         Fail    -> variant 5
   
-- Use smart constructors here
arbAnnRegExp :: Arbitrary a => Int -> Gen (RegExp a)
arbAnnRegExp 0 = oneof [ liftM Step arbitrary, return Succeed, return Fail ]
arbAnnRegExp n = oneof [ liftM2 (<*>) rec rec, liftM2 (<|>) rec rec
                       , arbAnnRegExp 0, liftM star rec
                       ]
 where rec = arbAnnRegExp (n `div` 2)
 
--------------------------------------------------------
-- QuickCheck properties

qq = star (((Succeed <|> Succeed) <|> Succeed) <|> (Succeed <|> Succeed)) <*> (Step 0 <|> Succeed)                                                                   

propSound :: RegExp Int -> Property
propSound p = not (null xs) ==> collect (length xs) $ all (member p) xs
 where xs = take 20 $ language p

propNonSucceed :: RegExp Int -> Bool
propNonSucceed p = not $ member (nonSucceed p) []

propSplitSucceed :: RegExp Int -> Bool
propSplitSucceed p = p === if isSucceed p then Succeed <|> new else new
 where new = nonSucceed p

propFirsts :: RegExp Int -> Bool
propFirsts p = nonSucceed p === foldr op Fail (firsts p)
 where op (a, q) r = (Step a <*> q) <|> r

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
propStar         p    =  star p === Succeed <|> (p <*> star p)

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
   quickCheck $ unit (<|>) Fail
   quickCheck $ associative (<*>)
   quickCheck $ unit (<*>) Succeed

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