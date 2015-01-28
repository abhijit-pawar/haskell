{-#OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseError :: [String] -> LogMessage
parseError (error:value:timestamp:xs) = LogMessage (Error (read value::Int)) (read timestamp::Int) (unwords xs) 

parseInfo :: [String] -> LogMessage
parseInfo (info:timestamp:xs) = LogMessage Info (read timestamp::Int) (unwords xs)


parseWarning :: [String] -> LogMessage
parseWarning (warning:timestamp:xs) = LogMessage Warning (read timestamp::Int) (unwords xs)


parseWrapper :: [String] -> LogMessage
parseWrapper log = case (head log) of
			"I" -> parseInfo log
			"W" -> parseWarning log
			"E" -> parseError log
--			_  ->  Unknown head.words log

--define a function to parse the individual line from the log file

parseMessage :: String -> LogMessage
parseMessage log = (parseWrapper.words) log


{-- parse the entire log file at once and return the contents as list of the 
    log messages
--}

parse :: String -> [LogMessage]
parse log =  map parseMessage $ lines log

--define a function which will insert the LogMessage in the binary tree where
-- the timestamp on left tree of the node is less and greater on the right tree.

insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined
