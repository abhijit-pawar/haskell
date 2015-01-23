--Tower of Hanoi

--This pillar which are used in movement
type Peg = String

--The move from one pillar to another pillar
type Move = (Peg,Peg)

{--
	Hanoi - Take the total disks and the 3 pillars and return the moves
	to be performed for the process to succeed
--}

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi totalDisks pegA pegB pegC  
			| totalDisks == 1 = [(pegA,pegB)]
			| totalDisks > 1 = hanoi (totalDisks - 1) pegA pegC pegB ++ [(pegA,pegB)] ++ hanoi (totalDisks -1) pegC pegB pegA


main :: IO ()
main = do
	putStrLn "Tower of Hanoi of 3 towers"
	putStrLn "Enter the number of disks present"
	disks <- getLine
	let totalDisks = read disks :: Integer
	let result = hanoi totalDisks "A" "B" "C"
	putStrLn $ show result
	putStrLn "The total Moves taken = "
	putStrLn $  show (length result)


