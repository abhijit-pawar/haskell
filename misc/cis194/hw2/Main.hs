import LogAnalysis
import Log

main :: IO()
main = do
	putStrLn "Starting the log file parser"
	d <- readFile "error.log"
	putStrLn $ show (lines d)
	--testParse parse 10 "error.log"
	putStrLn "Finished parsing the log file"
