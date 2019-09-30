-- CptS 355 - Fall 2019 Assignment 2
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework
--
-- Name: David Ott 
-- Discussed with: Cyrus Khorram, Matthias McFarlane, Ian Pruett (TA)

module HW2
     where

{- 1-  merge2 & merge2Tail & mergeN - 22% -}
--merge2
merge2 :: Ord a => [a] -> [a] -> [a]
merge2 [] [] = []
merge2 (x:xs) [] = x:xs
merge2 [] (y:ys) = y:ys
merge2 (x:xs) (y:ys)
     | (x > y) = y:merge2 (x:xs) ys
     | otherwise = x:merge2 xs (y:ys)

--merge2Tail
merge2Tail :: Ord a => [a] -> [a] -> [a]
merge2Tail [] [] = []
merge2Tail (x:xs) (y:ys) = mergeHelper (x:xs) (y:ys) []
     where
          mergeHelper :: Ord a => [a] -> [a] -> [a] -> [a]
          mergeHelper [] [] accum = accum
          mergeHelper (x:xs) [] accum = revAppend accum (x:xs)
          mergeHelper [] (y:ys) accum = revAppend accum (y:ys)
          mergeHelper (x:xs) (y:ys) accum
               | (x <= y) = mergeHelper xs (y:ys) (x:accum)
               | otherwise = mergeHelper (x:xs) ys (y:accum)

--as defined in class
revAppend :: [a] -> [a] -> [a]
revAppend [] list = list
revAppend (x:xs) list = revAppend xs (x:list)

     
--mergeN
mergeN :: (Foldable t, Ord a) => t [a] -> [a]
mergeN xs = foldl merge2 [] xs

{- 2 - getInRange & countInRange - 18% -}

--getInRange
-- First:
--   Filter out the numbers from the given list that are not > v1. Lets call this new list list2
-- Second:
--   Filter out the numbers from list2 that are not < v2. Return this list
getInRange :: Ord a => a -> a -> [a] -> [a]
--                                 (     list2     )
getInRange v1 v2 iL = filter (<v2) (filter (>v1) iL)

--countInRange 
countInRange :: Ord a => a -> a -> [[a]] -> Int
countInRange v1 v2 iL = foldl (+) 0 (map length(map (getInRange v1 v2) iL))

--The following function also satisfies the requirements but the TA implied this may not be the method you were looking for
--countInRange v1 v2 iL = length(getInRange v1 v2 (mergeN iL))


{- 3 -  addLengths & addAllLengths - 18% -}
data LengthUnit =  INCH  Int | FOOT  Int | YARD  Int
                   deriving (Show, Read, Eq)
-- addLengths 
addLengths :: LengthUnit -> LengthUnit -> LengthUnit
addLengths (INCH x) (INCH y) = (INCH (x + y))
addLengths (INCH x) (FOOT y) = (INCH (x + 12*y))
addLengths (INCH x) (YARD y) = (INCH (x + 36*y))

addLengths (FOOT x) (INCH y) = (INCH (12*x + y))
addLengths (FOOT x) (FOOT y) = (INCH (12*x + 12*y))
addLengths (FOOT x) (YARD y) = (INCH (12*x + 36*y))

addLengths (YARD x) (INCH y) = (INCH (36*x + y))
addLengths (YARD x) (FOOT y) = (INCH (36*x + 12*y))
addLengths (YARD x) (YARD y) = (INCH (36*x + 36*y))

-- addAllLengths
addAllLengths :: Foldable t => [t LengthUnit] -> LengthUnit
addAllLengths iL = foldl addLengths (INCH 0) (map(foldl addLengths(INCH 0)) iL)

{-4 - sumTree and createSumTree - 22%-}

data Tree a = LEAF a | NODE a  (Tree a)  (Tree a) 
              deriving (Show, Read, Eq)
 
--sumTree
sumTree :: Num p => Tree p -> p
sumTree (LEAF x) = x
sumTree (NODE x t1 t2) = sumTree t1 + sumTree t2

--createSumTree
createSumTree :: Num a => Tree a -> Tree a
createSumTree (LEAF value) = LEAF value
createSumTree (NODE value left right) = NODE ((sumTree left) + (sumTree right)) (createSumTree left) (createSumTree right)

{-5 - foldListTree - 20%-}

data ListTree a = ListLEAF [a] | ListNODE [(ListTree a)]
                  deriving (Show, Read, Eq)

--              ----- f -----   base  --- t ----  
foldListTree :: (a -> a -> a) -> a -> ListTree a -> a
foldListTree f base (ListLEAF x) = foldl f base x --adds all values in x
foldListTree f base (ListNODE x) = foldl f base (map (foldListTree f base) x)

{- 6- Create two tree values :  Tree Integer  and  listTree a ;  Both trees should have at least 3 levels. -}
{- 
Tree Integer:
myTree = NODE 1 (NODE 2 (LEAF 4) (LEAF 5)) (NODE 3 (NODE 6 (LEAF 8) (LEAF 9)) (LEAF 7))

                (1) 
               /   \
             (2)    (3)
             / \    / \
           (4) (5)(6) (7)
                  / \
                (8) (9)     
     
ListTree:
myTree = ListNODE[ ListNODE[ ListNODE[ ListLEAF[2,4,6], ListLEAF[7,8,9]], ListLEAF [1,3,5]], ListNODE[ ListLEAF[], ListLEAF[13,14,15]]]

               ( )
            /       \
         ()           ()
       /    \       /    \
      ()  [1,3,5]  []  [13,14,15]
    /    \
[2,4,6] [7,8,9]

-}