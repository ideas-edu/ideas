{--------------------------------------------------- 
Copyright (c)        2005 - 2006 
Johan Jeuring and Harrie Passier
---------------------------------------------------}
module EquationsUtility where



-- Standard Haskell library
import List



{- Funtions to Select an element in a tuple -} 

fst3 (a,b,c)   = a
snd3 (a,b,c)   = b
trd3 (a,b,c)   = c
fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
trd4 (a,b,c,d) = c
fth4 (a,b,c,d) = d 



{- Example: rotates [1,2,3] = [(1,[2,3]), (2,[1,3]), (3,[1,2])] -}

rotates :: [a] -> [(a,[a])]
rotates ys = rotates' [] ys
 where
 rotates' xs []     = []
 rotates' xs (y:ys) = (y, (xs ++ ys)) : rotates' (xs ++ [y]) ys   



{- Example: twoCombi [1,2,3,4] = [(1,2),(1,3),(1,4),(2,3)(2,4),(3,4)] -}

twoCombi :: [a] -> [(a,a)]
twoCombi []     = []
twoCombi [x]    = []
twoCombi (x:xs) = [(x,x')| x'<-xs] ++ twoCombi xs  



{- Function singleton returns True if the list contains one element -}
 
singleton :: [a] -> Bool
singleton xs = not (null xs) && null (tail xs)


{- Function second returns the second element, if it exists, in a list -}

second :: [a] -> a
second xs = if length xs >=2 then sec xs else error "second error"
   where sec (x:y:zs) = y 


{- Functions to determine (non) emptyness of a list -}

empty, notEmpty :: [a] -> Bool
empty []    = True
empty _     = False 
notEmpty [] = False
notEmpty _  = True



{- Return all elements which are a member of s1 and not a member of s2 -}
{- Example diffBag [1,2,2] [2,3] = [1,2] -}

diffSet :: Eq a => [a] -> [a] -> [a]
diffSet s1 s2 = [el | el <- s1, not (elem el (intersect s1 s2))]



{- Return the elements which are a member of xs and not a member of ys,
   where the number of elements in xs and ys matters.                     
   Example diffBag [1,2,2] [2,3] = [1,2]. -} 

diffBag :: Eq a => [a] -> [a] -> [a]
diffBag []     ys = []
diffBag (x:xs) ys | x `elem`ys = diffBag xs (delete x ys)  
                  | otherwise  = x:diffBag xs ys    


{- Logical exor function -}

exor :: Bool -> Bool -> Bool
exor True  False = True
exor False True  = True
exor _     _     = False

safeHead :: [a] -> String -> a
safeHead xs  s =  if not (null xs) then head xs else error s

  