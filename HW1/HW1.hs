-- CptS 355 - Fall 2019 Assignment 1
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework

-- Name: David Ott
-- Discussed with Cyrus Khorram, Shaina Lucio, Ian (TA), Petr 


module HW1
     where
          
import Data.Char (toUpper)

-- 1. exists
exists :: Eq t => t -> [t] -> Bool
exists x [] = False
exists x (y:ys) = if (x == y) then True else (exists x ys)

-- b) the type signature contains Eq because we need to be able to compare variables of type t to values in the list 
--    to see if they are equal or not. The only comparison we need is (==) and (!=) and adding Eq allows us to make that comparison

-- 2. listUnion
-- Moves all non-duplicate values from (x:xs) -> (y:ys) then calls on itself again to remove all duplicates from (y:ys)
listUnion xs ys = let
     listUnionHelper :: Eq a => [a] -> [a] -> [a]
     listUnionHelper [] ys = ys
     listUnionHelper (x:xs) ys = if(exists x ys) then (listUnionHelper xs ys) else (listUnionHelper xs (x:ys))

     in listUnionHelper (listUnionHelper xs ys) []

-- 3. replace
replace :: (Eq t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
replace index value [] = []
replace (0) (value) (z:zs) = value:zs
replace index value (z:zs) = z:(replace (index-1) value zs)

-- 4. prereqFor
prereqFor :: Eq t => [(a, [t])] -> t -> [a]
prereqFor [] y = []
prereqFor (x:xs) y = if(exists y (snd x)) then ((fst x):(prereqFor xs y)) else (prereqFor xs y)

-- 5. isPalindrome
isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome (x:xs) = 
     if((length (x:xs)) == 1) then True
     else if(x == ' ') then (isPalindrome xs)
     else if((last xs) == ' ') then (isPalindrome (x:(init xs)))
     else if((toUpper x) == (toUpper (last xs))) then (isPalindrome (init xs))
     else False

-- 6. groupSumtoN
groupSumtoN :: (Ord a, Num a) => a -> [a] -> [[a]]
groupSumtoN max [] = []
groupSumtoN max (y:ys) = let
     sumHelper :: (Ord a, Num a) => a -> [a] -> [a] -> [[a]]
     sumHelper max [] groupList = [groupList] --if passed an empty list [a], return [[a]]
     sumHelper max (y:ys) groupList           --else
          | ((sum(groupList) + y) <= max) = sumHelper max ys (y:groupList)  
          | ((sum(groupList) + y) > max) = groupList:(sumHelper max ys [y])
     in listReverse(sumHelper max (y:ys) [])

-- Converts lists [[a]] that look like this: [[4,3,2,1],[6,5],[7],[8],[9],[10]]   
--                                into this: [[1,2,3,4],[5,6],[7],[8],[9],[10]]
listReverse :: [[a]] -> [[a]]
listReverse [] = []
listReverse (x:xs)
     | (length x == 1) = x:(listReverse xs)
     | otherwise = ((reverse x):(listReverse xs))