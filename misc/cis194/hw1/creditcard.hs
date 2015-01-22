

--find the digits of the number

toDigits :: Integer -> [Integer]
toDigits n
	| n == 0  = []
	| n < 0 = []
	| otherwise  = toDigits ( n `div` 10 ) ++ [n `mod` 10]

-- reverse the digits of the number
toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
	| n ==0  = []
	| n < 0 = []
	| otherwise = [n `mod` 10 ] ++ toDigitsRev (n `div` 10)

--double the every other number of the list

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:xs) = [x] ++ [2*y] ++ doubleEveryOther xs


--sum all the digits of the list.
sumDigits :: [Integer] -> Integer
sumDigits  = sum.(concat.(map toDigits))

--validate the credit card number
validate :: Integer -> Bool
validate val 
	| val `mod` 10 ==0 = True
	| otherwise = False
	 

main :: IO ()
main = do
	putStrLn "Validating the credit card number"
	putStrLn "Enter your credit card number"
	card <- getLine

	putStrLn "validating the number"
	let ret = validate $ sumDigits $ doubleEveryOther $ toDigitsRev (read card :: Integer)
	putStrLn $ show ret
