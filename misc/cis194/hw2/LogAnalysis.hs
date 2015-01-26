{-#OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


--define a function to parse the individual line from the log file

parseMessage :: String -> LogMessage
parseMessage = undefined


{-- parse the entire log file at once and return the contents as list of the 
    log messages
--}

parse :: String -> [LogMessage]
parse = undefined


main :: IO ()
main = putStrLn "Hello Log"
