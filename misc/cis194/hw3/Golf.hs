module Golf where

import Data.List

--Take the items from the list whose position is multiple of the given number
multiplesOf :: Int -> [a] -> [a]
multiplesOf _ [] = []
multiplesOf n xs = map fst $ filter (\(a,b) -> b `mod` n == 0) $ zip xs [1..]

--Excercise 1  Hopscotch
skips :: [a] -> [[a]]
skips xs = map (\l -> multiplesOf l xs) [1..length xs]



--Excercise 2 Local Maxima

getTuples :: Int -> [a] -> [[a]]
getTuples _ [] = []
getTuples n xs = filter(\ds -> length ds == n) $ (take n xs ) : getTuples n (drop 1 xs)

localMaxima :: [Integer] -> [Integer]
--localMaxima w:x:y:xs = map ( filter (\w x y -> w < x && x > y) w x y ) xs : localMaxima xs
localMaxima xs  = map (\(a:b:c:ds) -> b).filter (\(a:b:c:ds) -> a < b && b > c) $ getTuples 3 xs

--Excercise 3 Histogram

--Build the rows from the given list
--The list contain row number and the items on that row
buildRow :: [Int] -> [[Int]]
buildRow = transpose.reverse.sort.group.sort

histogram :: [Integer] -> String
histogram xs = "\n==========\n0123456789\n"


