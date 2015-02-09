module Golf where

--Take the items from the list whose position is multiple of the given number
multiplesOf :: Int -> [a] -> [a]
multiplesOf _ [] = []
multiplesOf n xs = map fst $ filter (\(a,b) -> b `mod` n == 0) $ zip xs [1..]

--Excercise 1  Hopscotch
skips :: [a] -> [[a]]
skips xs = map (\l -> multiplesOf l xs) [1..length xs]



--Excercise 2 Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima = undefined

--Excercise 3 Histogram
histogram :: [Integer] -> String
histogram = undefined


