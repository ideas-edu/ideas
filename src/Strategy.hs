-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Strategy 
   ( Strategy, IsStrategy(..)
   , (<*>), (<|>), (|>), succeed, failS, seqList, altList, repeatS, try, exhaustive, somewhere, somewhereTD
   , runStrategy, nextRule, nextRulesWith, isSucceed, isFail, trackRule, trackRulesWith
   , intermediates, intermediatesList, Label, label, labeledStrategies, check
   , checks
   ) where

import Transformation
import Move
import Utils
import Test.QuickCheck hiding (check, label)
import Control.Monad
import Control.Arrow
import Data.Maybe
import Data.List
import Data.Char

-----------------------------------------------------------
--- Data type

data Strategy a = Seq      (Strategy a) (Strategy a)
                | Alt      (Strategy a) (Strategy a)
                | Step     (Rule a)
                | Ann      Annotation   (Strategy a)  -- annotations on the strategy
                | Succeed 
                | Fail
 deriving Show

data Annotation = Recursion | Label Label
 deriving Show

type Label = String

-----------------------------------------------------------
--- Strategy type class

class IsStrategy s where
   toStrategy :: s a -> Strategy a

instance IsStrategy Strategy where
   toStrategy = id
   
instance IsStrategy Rule where
   toStrategy = Step

-----------------------------------------------------------
--- Smart constructors

infixr 5 <*>
infixr 4 |>, <|>
 
(<*>), (<|>), (|>) :: (IsStrategy s, IsStrategy t) => s a -> t a -> Strategy a
p <*> q = toStrategy p `Seq` toStrategy q
p <|> q = toStrategy p `Alt` toStrategy q
p  |> q = p <|> (notS p <*> q)

succeed, failS :: Strategy a
succeed = Succeed
failS   = Fail

seqList, altList :: IsStrategy s => [s a] -> Strategy a
seqList = foldr (<*>) succeed
altList = foldr (<|>) failS

-- greedy!
repeatS :: IsStrategy s => s a -> Strategy a
repeatS p = try (p <*> Ann Recursion (repeatS p))

try :: IsStrategy s => s a -> Strategy a
try p = p |> succeed

check :: (a -> Bool) -> Strategy a
check p = toStrategy (makeRule "Check" $ \a -> if p a then Just a else Nothing)

notS :: IsStrategy s => s a -> Strategy a
notS s = check (\a -> not $ applicable (toStrategy s) a)

exhaustive :: IsStrategy s => [s a] -> Strategy a
exhaustive = repeatS . altList . map toStrategy

somewhere :: (IsStrategy s, Move a) => s a -> Strategy a        
somewhere p = ruleMoveTop <*> rec
 where
   f rule = rule <*> Ann Recursion rec
   rec    = f ruleMoveLeft <|> f ruleMoveRight <|> f ruleMoveDown <|> p

-- top/down
somewhereTD :: (IsStrategy s, Move a) => s a -> Strategy a        
somewhereTD p = ruleMoveTop <*> rec
 where
   f rule = rule <*> Ann Recursion rec
   rec    = p <|> f ruleMoveLeft <|> f ruleMoveRight <|> f ruleMoveDown
   
-----------------------------------------------------------
--- Evaluation

runStrategy :: Strategy a -> a -> [a]
runStrategy strategy a =
   case strategy of
      Succeed -> [a]
      Fail    -> []
      Step r  -> applyM r a
      Ann _ p -> runStrategy p a
      Alt p q -> runStrategy p a ++ runStrategy q a
      Seq p q -> [ c | b <- runStrategy p a, c <- runStrategy q b ]

instance Apply Strategy where
   apply strategy a = 
      case runStrategy strategy a of
         []   -> Nothing
         hd:_ -> Just hd 

isSucceed :: Strategy a -> Bool
isSucceed strategy =
   case strategy of
      Succeed -> True
      Alt p q -> isSucceed p || isSucceed q
      Seq p q -> isSucceed p && isSucceed q
      Ann _ p -> isSucceed p
      _       -> False

isFail :: Strategy a -> Bool
isFail strategy =
   case strategy of
      Fail    -> True
      Alt p q -> isFail p && isFail q
      Seq p q -> isFail p || (isSucceed p && isFail q) -- weakened for infinite strategies
      Ann _ p -> isFail p
      _       -> False

firsts :: Strategy a -> [(Rule a, Strategy a)]
firsts strategy =
   case strategy of
      Succeed -> []
      Fail    -> []
      Ann _ p -> firsts p
      Step r  -> [(r, succeed)]
      Alt p q -> firsts p ++ firsts q
      Seq p q -> map (second (<*> q)) (firsts p) ++ if isSucceed p then firsts q else []

nextRule :: Strategy a -> a -> [(Rule a, a, Strategy a)]
nextRule strategy a = [ (r, b, s) | (r, s) <- firsts strategy, b <- applyM r a ]

nextRulesWith :: (Rule a -> Bool) -> Strategy a -> a -> [([Rule a], a, Strategy a)]
nextRulesWith p strategy a = concatMap f (nextRule strategy a)
 where f (r, b, s) | p r       = [([r], b, s)]
                   | otherwise = [ (r:rs, c, t) | (rs, c, t) <- nextRulesWith p s b ] 

trackRule :: Rule a -> Strategy a -> Strategy a
trackRule rule = 
   altList . map snd . filter ((==rule) . fst) . firsts

trackRulesWith :: (Rule a -> Bool) -> Rule a -> Strategy a -> a -> Strategy a
trackRulesWith p rule strategy = 
   altList . map thd3 . filter ((==rule) . last .  fst3) . nextRulesWith p strategy

intermediates :: Strategy a -> a -> [([Rule a], a, Strategy a)]
intermediates strategy = concat . intermediatesList strategy

-- list of results reflect the search depth
intermediatesList :: Strategy a -> a -> [[([Rule a], a, Strategy a)]]
intermediatesList strategy a = takeWhile (not . null) (iterate (concatMap next) start)
 where
   start = [([], a, strategy)]
   next (rs, a, s) = [ (r:rs, b, ns) | (r, b, ns) <- nextRule s a ]

-- returns a strategy without the Succeed alternative
nonSucceed :: Strategy a -> Strategy a
nonSucceed strategy =
   case strategy of
      Seq p q
         | isSucceed p && isSucceed q -> p <|> q <|> (nonSucceed p <*> nonSucceed q)
         | otherwise -> strategy
      Alt p q -> nonSucceed p <|> nonSucceed q 
      Step _  -> strategy
      Ann a p -> Ann a (nonSucceed p)
      _       -> Fail

-----------------------------------------------------------
--- Labels

label :: IsStrategy s => Label -> s a -> Strategy a
label l = Ann (Label l) . toStrategy

-- in sequence only
labeledStrategies :: Strategy a -> [(Label, Strategy a)]
labeledStrategies strategy =
   case strategy of
      Seq p q         -> labeledStrategies p ++ labeledStrategies q
      Ann (Label l) p ->[(l, p)]
      Ann _         p -> labeledStrategies p
      _               -> []
            
-----------------------------------------------------------
--- QuickCheck generator

instance Arbitrary a => Arbitrary (Strategy a) where
   arbitrary = sized arbStrategy
   coarbitrary (Seq s t) = variant 0 . coarbitrary s . coarbitrary t
   coarbitrary (Alt s t) = variant 1 . coarbitrary s . coarbitrary t
   coarbitrary (Step r)  = variant 2 . coarbitrary r
   coarbitrary (Ann a r) = variant 3 . coarbitrary a . coarbitrary r
   coarbitrary Succeed   = variant 4
   coarbitrary Fail      = variant 5

arbStrategy :: Arbitrary a => Int -> Gen (Strategy a)
arbStrategy 0 = frequency [(5, liftM Step arbitrary), (1, return Succeed), (1, return Fail)]
arbStrategy n = oneof [arbStrategy 0, liftM2 Ann arbitrary (arbStrategy h), binop Alt, binop Seq]
 where h = n `div` 2
       binop op = liftM2 op (arbStrategy h) (arbStrategy h)

instance Arbitrary Annotation where
   arbitrary = oneof [return Recursion, liftM f arbitrary ]
    where f :: Int -> Annotation
          f n = Label ("l" ++ show n)
   coarbitrary Recursion = variant 0
   coarbitrary (Label l) = variant 1 . coarbitrary (map ord l)
       
-----------------------------------------------------------
--- QuickCheck properties: algebraic laws for strategies

checks :: IO ()
checks = do
   mapM_ quickCheck [prop1, prop2]
   mapM_ quickCheck [prop3, prop4]
   mapM_ quickCheck [prop5, prop6, prop7, prop8, prop9, prop10]
   quickCheck propSucceed
   quickCheck propFail
   quickCheck propNonSucceed
   quickCheck propFirsts
   quickCheck propNext
   quickCheck propTrack

infix 1 ===, ~==

(===), (~==) :: Strategy Int -> Strategy Int -> Int -> Property
p === q = \a -> property (runStrategy p a == runStrategy q a)
p ~== q = \a -> property ((nub $ sort $ runStrategy p a) == (nub $ sort $ runStrategy q a))

associative op p q r = p `op` (q `op` r) === (p `op` q) `op` r

-- associativity laws
prop1 = associative (<|>)
prop2 = associative (<*>)

-- distributivity laws
prop3, prop4 :: Strategy Int -> Strategy Int -> Strategy Int -> Int -> Property
prop3 p q r = p <*> (q <|> r)  ~==  (p <*> q) <|> (p <*> r)
prop4 p q r = (p <|> q) <*> r  ===  (p <*> r) <|> (q <*> r)

-- neutral elements
prop5  p = p <*> succeed  ===  p
prop6  p = succeed <*> p  ===  p
prop7  p = p <*> failS    ===  failS
prop8  p = failS <*> p    ===  failS
prop9  p = failS <|> p    ===  p
prop10 p = p <|> failS    ===  p
    
-- properties for strategy operations
propSucceed :: Strategy Int -> Int -> Property
propSucceed s a = isSucceed s ==> a `elem` runStrategy s a 

propFail :: Strategy Int -> Int -> Property
propFail s a = isFail s ==> null (runStrategy s a) 

propNonSucceed :: Strategy Int -> Property
propNonSucceed s = isSucceed s ==> s ~== (Succeed <|> nonSucceed s) 
      
propFirsts :: Strategy Int -> Property
propFirsts s =
   let xs = map f (firsts s)
       f (r, s) = r <*> s
   in not (isSucceed s) ==> s ~== altList xs

propNext:: Strategy Int -> Int -> Property
propNext s a =
   let xs = map f (nextRule s a)
       f (r, _, s) = r <*> s
   in not (isSucceed s) ==> (s ~== altList xs) a
 
propTrack :: Strategy Int -> Int -> Property
propTrack s a =
   let list      = nextRule s a
       (r, b, t) = head list
   in not (null list) ==>
      (t ~== trackRule r s) b
         
-------------------------
{-
-- import qualified UU.Parsing as P

instance P.Symbol (Rule a) where
   deleteCost = const 100000
   symBefore  = id
   symAfter   = id

translate :: Strategy a -> P.Parser (Rule a) ()
translate strategy = 
   case strategy of
      Succeed -> P.pSucceed ()
      Fail    -> P.pFail
      Seq p q -> translate p P.<* translate q
      Alt p q
         | isSucceed p && isSucceed q -> translate (succeed <|> nonSucceed strategy)
         | otherwise -> translate p P.<|> translate q
      Step r  -> () P.<$ P.pSym r
      
propUU :: Strategy Int -> Bool
propUU s = expToList (P.getfirsts (translate s)) ~= map fst (firsts s)
 where xs ~= ys = isSubsetOf xs ys && isSubsetOf ys xs

propUUFirsts :: Strategy Int -> Bool
propUUFirsts s = map fst (firsts s) `isSubsetOf` expToList (P.getfirsts (translate s))

expToList :: Eq a => P.Expecting a -> [a]
expToList (P.ESym (P.Range s1 s2)) = [s1 | s1==s2]
expToList (P.EOr xs) = concatMap expToList xs
expToList (P.ESeq xs) = concatMap expToList $ take 1 xs
expToList _ = [] -}