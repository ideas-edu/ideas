module Service.Progress
   ( Progress, emptyProgress, success, failure, try
   , fromList, fromMaybeList
   , scoreList, scoreListMonotonic
   , scoreMaybeList, scoreMaybeListMonotonic
   , (<|>), mergeList, (<*>), combineBy, combineList
   , mapProgress, concatProgress, filterProgress
   , addScore, maxDepth, maxNumber, successes, failures, (<||>), extractFirst
   , successesForScore, accumProgress, scoreMaybeLists, scoreMaybeListMonotonics, fromListBy
   , fromMap
   ) where

import Control.Monad
import Data.List (groupBy, sortBy)
import qualified Data.Map as M
   
----------------------------------------------------------
-- The Progress data type
--    invariant: scores in list are strictly increasing, and all >0

newtype Progress score a = P { unP :: [Step score a] }

data Step score a = Score score | Success a | Failure

----------------------------------------------------------
-- Instances
--    the Monad instance is defined in terms of map and concat 

instance Functor (Progress score) where
   fmap = mapProgress

instance (Num score, Ord score) => Monad (Progress score) where
   return  = success
   p >>= f = concatProgress (mapProgress f p)

instance (Show score, Num score, Show a) => Show (Progress score a) where
   show (P xs) =
      let reportScore i = "*** Score: " ++ show i
          reportFails i = "(Failures: " ++ show i++")\n"
          f n (Failure  :rest) = f (n+1) rest
          f n (Score i  :rest) = reportFails n : reportScore i : f 0 rest
          f n (Success a:rest) = show a : f n rest 
          f n []               = [reportFails n]          
      in unlines (reportScore 0 : f 0 xs)

----------------------------------------------------------
-- Constructor functions 

emptyProgress :: Progress score a
emptyProgress = P []

success :: a -> Progress score a
success a = P [Success a]

failure :: Progress score a
failure = P [Failure]

try :: Maybe a -> Progress score a
try = maybe failure success

----------------------------------------------------------
-- Constructing a Progress from a list

fromList :: [a] -> Progress score a
fromList = fromMaybeList . map Just

fromMaybeList :: [Maybe a] -> Progress score a
fromMaybeList = P . map (maybe Failure Success)

fromListBy :: (Num score, Ord score) => (a -> score) -> [a] -> Progress score a
fromListBy f = scoreList . map (\x -> (f x, x))

scoreList, scoreListMonotonic :: (Num score, Ord score) => [(score, a)] -> Progress score a
scoreList = scoreListMonotonic . sortByFst
scoreListMonotonic = scoreMaybeListMonotonic . map (\(i, a) -> (i, Just a)) 

scoreMaybeList, scoreMaybeListMonotonic :: (Num score, Ord score) => [(score, Maybe a)] -> Progress score a
scoreMaybeList = scoreMaybeListMonotonic . sortByFst
scoreMaybeListMonotonic = scoreMaybeListMonotonics . groupByFst

scoreMaybeLists, scoreMaybeListMonotonics :: (Num score, Ord score) => [(score, [Maybe a])] -> Progress score a
scoreMaybeLists = scoreMaybeListMonotonics . sortByFst 

scoreMaybeListMonotonics = 
   let g (i, xs) = (i, [ Score i | i > 0 ] ++  map (maybe Failure Success) xs)
       safeConcat n [] = []
       safeConcat n ((i, xs):rest)
          | i > n     = xs ++ safeConcat i rest
          | otherwise = error "scoreMaybeListMonotonics: list not sorted"
   in P . safeConcat (-1) . map g

{-
-- the list should be sorted
listToProgress :: (Num score, Ord score) => [(score, a)] -> Progress score a
listToProgress = 
   let eqFst a b = fst a == fst b
       g as@((i, _):_) = (i, [ Score i | i > 0 ] ++  map (Success . snd) as)
       safeConcat n [] = []
       safeConcat n ((i, xs):rest)
          | i > n     = xs ++ safeConcat i rest
          | otherwise = error "listToProgress: list not sorted"
   in P . safeConcat (-1) . map g . groupBy eqFst

listToProgressBy :: (Num score, Ord score) => (a -> score) -> [a] -> Progress score a
listToProgressBy f = listToProgress . map (\a -> (f a, a))-}

----------------------------------------------------------
-- Merging progress monads
--    (Progress is not a MonadPlus because the right identity law is not satisfied)

instance (Num score, Ord score) => MonadPlus (Progress score) where
   mzero = emptyProgress
   mplus = (<|>)

infixr 2 <|>, .|.

(<|>) :: Ord score => Progress score a -> Progress score a -> Progress score a
P xs <|> P ys = P (xs .|. ys) 

mergeList :: Ord score => [Progress score a] -> Progress score a
mergeList = foldr (<|>) emptyProgress

-- local helper-function
(.|.) :: Ord score => [Step score a] -> [Step score a] -> [Step score a]
xs .|. ys = f xs
 where
   f [] = ys
   f list1@(hd1@(Score i1):tl1) = g ys
    where
      g [] = list1
      g list2@(hd2@(Score i2):tl2)
         | i1 < i2        = hd1 : (tl1 .|. list2)
         | i1 > i2        = hd2 : (list1 .|. tl2)
         | otherwise      = hd1 : (tl1 .|. tl2)
      g (hd2:tl2) = hd2:g tl2
   f (hd1:tl1) = hd1: f tl1

{-
   case (xs, ys) of
      ([], _) -> ys
      (_, []) -> xs 
      (hd1@(Score i1):tl1, hd2@(Score i2):tl2) 
         | i1 < i2        -> hd1 : (tl1 .|.    ys)
         | i1 > i2        -> hd2 : (   xs .|. tl2)
         | otherwise      -> hd1 : (tl1 .|. tl2)
      (
     
      (hd1@(Success a):tl1, _) -> hd1 : (tl1 .|.   ys)
      (hd1@Failure  :tl1, _) -> hd1   : (tl1 .|.   ys)
      (_, hd2@(Success a):tl2) -> hd2 : (  xs .|. tl2)
      (_, hd2@Failure  :tl2) -> hd2   : (  xs .|. tl2)
    -}
 
 --      (hd:tl, _)          -> hd : (tl .|. ys)
--      (_, hd:tl)          -> hd : (xs .|. tl)
--      _                   -> []
--      ([], _)             -> ys
--      (_, [])             -> xs

----------------------------------------------------------
-- Combining progress monads
--    (Defined using the Monad instance)

infixr 3 <*>

combineBy :: (Num score, Ord score) => (a -> b -> c) -> Progress score a -> Progress score b -> Progress score c
combineBy f pa pb = pa >>= \a -> pb >>= \b -> return (f a b)

(<*>) :: (Num score, Ord score) => Progress score a -> Progress score b -> Progress score (a, b)
(<*>) = combineBy (,)

combineList :: (Num score, Ord score) => [Progress score a] -> Progress score [a]
combineList = foldr (combineBy (:)) (success [])
      
----------------------------------------------------------
-- map and concat (also see the Monad instance)
 
mapProgress :: (a -> b) -> Progress score a -> Progress score b
mapProgress f (P xs) =
   let g (Score i)   = Score i
       g (Success a) = Success (f a)
       g Failure     = Failure
   in P (map g xs)   
   
concatProgress :: (Num score, Ord score) => Progress score (Progress score a) -> Progress score a
concatProgress (P xs) = 
   let f n (Score i  :rest) = [Score i | i /= n] ++ f i rest
       f n (Failure  :rest) = Failure : f n rest
       f n (Success p:rest) = plusScore n (unP p) .|. f n rest
       f _ [] = []
   in P (f 0 xs)

----------------------------------------------------------
-- filtering the successes (with introducing Failure steps)

filterProgress :: (Ord score, Num score) => (a -> Bool) -> Progress score a -> Progress score a
filterProgress p = 
   let f a = if p a then return a else failure 
   in concatProgress . fmap f

accumProgress :: (a -> b -> b) -> b -> Progress score a -> Progress score (a, b)
accumProgress op e (P xs) = 
   let accum b (Success a:rest) = Success (a, b) : accum (op a b) rest
       accum b (Failure  :rest) = Failure : accum b rest
       accum b (Score i  :rest) = Score i : accum b rest
       accum _ []               = []
   in P (accum e xs)

----------------------------------------------------------
-- Increase the score  

addScore :: (Num score, Ord score) => score -> Progress score a -> Progress score a 
addScore i (P xs) = -- improved?
   let f (Score j:_) = [Score i | i+j > 0]
       f _           = [Score i | i > 0]
   in P (f xs ++ plusScore i xs)
   
{-
   let f (Score _:_) = [Score i | i > 0]
       f _           = []
   in P (f xs ++ plusScore i xs)
  -} 
----------------------------------------------------------
-- Limit the size

-- number of failures plus successes
maxNumber :: Int -> Progress score a -> Progress score a
maxNumber i (P xs) = 
   let f 0 _            = []
       f n (Score i:xs) = Score i:f n xs
       f n (x:xs)       = x:f (n-1) xs          
       f n []           = []
   in P (f i xs)
   
maxDepth :: Ord score => score -> Progress score a -> Progress score a
maxDepth score (P xs) =
   let lowScore (Score i) = i <= score
       lowScore _         = True
   in P (takeWhile lowScore xs)

----------------------------------------------------------
-- Local auxiliary function

plusScore :: Num score => score -> [Step score a] -> [Step score a]
plusScore i = 
   let f (Score j) = Score (i+j)
       f x         = x   
   in map f

successesForScore :: Progress score a -> [(score, [a])]
successesForScore (P xs) = 
   let f ms xs []  = if null xs then [] else maybe [] (\i -> [(i, reverse xs)]) ms
       f ms xs (Score j:ys) = let rest = f (Just j) [] ys
                                  this = if null xs then [] else maybe [] (\i -> [(i, reverse xs)]) ms
                              in this ++ rest
       f ms xs (Success a:ys) = f ms (a:xs) ys
       f ms xs (_:ys) = f ms xs ys
   in f Nothing [] xs

successes :: Progress score a -> [a]
successes (P xs) = [ a | Success a <- xs]

failures :: Progress score a -> Int
failures (P xs) = length [ () | Failure <- xs]

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\a b -> fst a `compare` fst b)

groupByFst :: Ord a => [(a, b)] -> [(a, [b])]
groupByFst = map f . groupBy eqFst
 where
   f xs = (fst $ head xs, map snd xs)
   eqFst (x, _) (y, _) = x == y
   
(<||>) :: (Ord score, Num score) => Progress score a -> Progress score a -> Progress score a              
P xs <||> P ys = P $ rec 0 xs
 where
   rec _ (step@(Score c):rest) = step:rec c rest
   rec c (step:rest)    = step:rec c rest
   rec c [] = 
      case ys of
         [] -> []
         Score d:rest
            | c > d  -> error "<||>"
            | c == d -> rest
            | c < d  -> ys
         _ -> if c == 0 then ys else error "<||>"
         
extractFirst :: (Ord score, Num score) => Progress score a -> Maybe (score, a, Int, Progress score a)
extractFirst (P xs) = rec 0 0 xs
 where
   rec _ _ []               = Nothing
   rec _ f (Score s  :rest) = rec s f rest
   rec s f (Success a:rest) = Just (s,a,f, P ([Score s | s > 0] ++ rest))
   rec s f (Failure  :rest) = rec s (f+1) rest
   
fromMap :: (Num score, Ord score) => M.Map score [a] -> Progress score a
fromMap = scoreMaybeListMonotonics . M.assocs . M.map (map Just)