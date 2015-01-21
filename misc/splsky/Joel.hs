--Abhijit Chandrakant Pawar ( abhi.c.pawar@gmail.com )
{--
This program is an attempt to get answer to the question posed by joel software once. 
I am able to code it correctly in haskell however still there are chances of improvements which I would be doing sooner.
--} 

import Data.List as L
import Data.Maybe as MB

myMap :: (a->b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

createReverseList :: Int -> [Int]
createReverseList 0 = []
createReverseList x = x : createReverseList (x-1)

getIndex :: Char -> Maybe Int
getIndex c = c `elemIndex` "acdegilmnoprstuw" 

getHash :: [Char] -> [Maybe Int]
getHash str = myMap getIndex str

calcHash :: [Int] -> Int
calcHash  =  L.foldl (\h x -> h*37 +  x) 7 

getTempValues ::Int -> [Int] -> [Int]
getTempValues hash   =   myMap ((\h x -> h `div` 37^x ) hash ) 

getNo:: Int -> Int -> Int -> Int
getNo hash x y = (hash `div` 37^(y-1)) - x*37

--get the indices for the hash and string of specified length
getIndices :: Int -> Int -> [Int] -> [Int]
getIndices hash 0 _ = []
getIndices hash i (x:xs) = getNo hash x i : getIndices hash (i-1) xs 

--build the final name list from the indices which we have received.
buildNameList :: [Char] -> [Int] -> [Char]
buildNameList _ [] = []
buildNameList xs (y:ys) = xs!!y : buildNameList xs ys

main:: IO ()
main = do
		putStrLn "joel's puzzle"
		let elementList = "acdegilmnoprstuw"
		let givenHash =  910897038977002 --680131659347 910897038977002 --
		let givenStringLen = 9 --7  
		let stringName = buildNameList elementList $ getIndices givenHash givenStringLen $ [7] ++ getTempValues givenHash (createReverseList (givenStringLen -1))
		let recalcluatedhash = calcHash $ MB.catMaybes $ getHash stringName
		print stringName
		print recalcluatedhash
