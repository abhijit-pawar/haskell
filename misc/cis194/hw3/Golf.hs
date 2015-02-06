module Golf where

myTake :: String -> Int -> Char 
myTake str int = (\y x -> y!!x ) str int

mine :: String -> Int-> String
mine str int = (\i->(\ls z-> map((\x -> myTake x) ls) $ (\listLength -> take listLength. filter ((\y x -> x `mod` y == 0) z) $ [1..listLength -1] ).length $ ls) i) str int

--Excercise 1 Hopscotch
skips :: String -> [String]
skips ls = (\l-> map (mine ("_"++ls)) [1..l+1]) $ length $ ls


--Excercise 2 Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima = undefined

--Excercise 3 Histogram
histogram :: [Integer] -> String
histogram = undefined


