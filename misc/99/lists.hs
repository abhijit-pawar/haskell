--Abhijit Chandrakant Pawar ( abhi.c.pawar@gmail.com )
{--
The samples from 99 haskell problems
--} 
import System.Environment

myLast :: [a] -> a

myLast [x] = x
myLast (_:xs) = myLast xs

--greet xs = "\nHello, " ++ xs
myLast' = head.reverse


lastButOne = head.tail.reverse

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)


myLength :: [a] -> Int
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

--mylen = foldl (\n _ -> n + 1) 0
mylen = foldr (\_ n -> n + 1) 0


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)  


data NestedList a = Elem a | List [NestedList a]

myflatten :: NestedList a -> [a]
myflatten (Elem a) = [a]
myflatten  (List (x:xs)) = myflatten x ++ myflatten (List xs)
myflatten (List []) = []


insertAt :: a -> [a] -> Int -> [a]
insertAt x (ys) 1 = x:ys
insertAt x (y:ys) n = y:insertAt x ys (n -1)

main :: IO ()
main = do
   putStrLn $ unlines ["Welcome to the haskell programs."]
   --fmap greet getLine >>= putStrLn
   let z = insertAt 'C' "NewTest" 5
--   let z = myflatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
   print z 
