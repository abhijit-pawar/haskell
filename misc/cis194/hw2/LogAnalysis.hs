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
			_  ->  Unknown (unwords log)



parseMessage :: String -> LogMessage
parseMessage log = (parseWrapper.words) log


{-- parse the entire log file at once and return the contents as list of the 
    log messages
--}

parse :: String -> [LogMessage]
parse log =  map parseMessage $ lines log


getLogMsgTimeStamp :: LogMessage -> Int
getLogMsgTimeStamp ( LogMessage m t d ) = t


getMsgTreeTimeStamp :: MessageTree -> Int
getMsgTreeTimeStamp (Node l t r) =  getLogMsgTimeStamp t


insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsgInput (Node leftTree treeLog rightTree)  
						| inTimeStamp == msgTimeStamp = Node leftTree logMsgInput rightTree
						| inTimeStamp < msgTimeStamp = Node (insert logMsgInput leftTree) treeLog rightTree
						| inTimeStamp > msgTimeStamp = Node leftTree treeLog (insert logMsgInput rightTree)
						where
							inTimeStamp = getLogMsgTimeStamp logMsgInput
							msgTimeStamp = getMsgTreeTimeStamp (Node leftTree treeLog rightTree)


--build the messageTree
build :: [LogMessage] -> MessageTree
build = foldr insert (Node Leaf (LogMessage Info 3000 "test") Leaf)



