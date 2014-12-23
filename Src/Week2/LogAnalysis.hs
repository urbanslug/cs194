{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Src.Week2.Log
import Data.List
import Data.Char

parseMessage :: String -> MaybeLogMessage
parseMessage contents =
  case (words contents) of
    ("I":y:ys) -> ValidLM $ LogMessage Info (read y) (unwords ys)
    ("W":y:ys) ->
      ValidLM $ LogMessage Warning (read y) (unwords ys)
    ("E":z:y:ys) ->
      ValidLM $ LogMessage (Error (read z)) (read y) (unwords ys)
    list -> InvalidLM $ unwords list

parseErrorFile :: String -> [MaybeLogMessage]
parseErrorFile [] = []
parseErrorFile contents =
  let (x:xs) = lines contents
  in parseMessage x : parseErrorFile (unlines xs)

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly xs = [ x | ValidLM x <- xs ]
{-
Alternative implementation:

validMessagesOnly [] = []
validMessagesOnly (x:xs) = case x of
  InvalidLM _ -> validMessagesOnly xs
  ValidLM y -> y : validMessagesOnly xs
-}

parse :: String -> [LogMessage]
parse contents = validMessagesOnly $ parseErrorFile contents

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ x _) (LogMessage _ y _) = compare x y


sortMessages :: [LogMessage] -> [LogMessage]
sortMessages messages = sort messages

{-
Alternative implementation:

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages x =  sortBy compareMsgs x
-}

getMessageType :: LogMessage -> MessageType
getMessageType (LogMessage messageType _ _) = messageType

getMessageItself :: LogMessage -> String
getMessageItself (LogMessage _ _ messageItself) = messageItself

getErrorMessages :: [LogMessage] -> [LogMessage]
getErrorMessages listOfLogs =
  [x | x <- listOfLogs, Error _ <- [getMessageType x]]

{-
Alternative implementation:

getErrorMessages listOfLogs = filter (\x -> case getMessageType x of Error _ -> True; _ -> False) listOfLogs

isError (Error _) = True; isError _ = False
getErrorMessages listOfLogs = filter (isError . getMessageType) listOfLogs
-}

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong listOfLogs =
  reverse $ map getMessageItself (take 50 (sort (getErrorMessages listOfLogs)))



messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout str logMessages =
  filter (\x ->
           isInfixOf (map toLower str) ((map toLower (getMessageItself x))))
  logMessages


whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced str listOfLogs =
  whatWentWrong $ messagesAbout str listOfLogs

(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x -- (||) is Haskell’s ordinary "or" operator
