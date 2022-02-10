module Main where

import Data.Char
import Control.Concurrent
import System.Environment   
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit (simpleHttp)
import Data.List.Split
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.ByteString.Lazy.UTF8 (toString) 
import System.IO

data GeneratorState = IsRunning | IsFinished
  deriving (Eq)

isChanged :: Diff f -> Bool
isChanged (First _) = True
isChanged _ = False

hasDiff :: [a] -> Bool
hasDiff [] = False
hasDiff _ = True

-- Changes as one string if they exsists
getChanges :: [String] -> [String] -> Maybe String
getChanges now before = if (hasDiff ch) then Just res else Nothing
  where ch = filter isChanged diff
        diff = getDiff now before
        res = foldr (\(First x) y -> x ++ " \n " ++ y) "" ch

-- TODO
-- Should return signal to stop the script if there's a checkbox with a keyword `stop`
showRunning :: [String] -> GeneratorState
showRunning html = IsRunning

-- Printing time diff in %H:%M:%S format
myFormatDiffTime :: (UTCTime, UTCTime) -> String
myFormatDiffTime (a,b)= formatTime defaultTimeLocale "%H:%M:%S" . posixSecondsToUTCTime $ diffUTCTime  a b

-- Printing changes is they exsists
genOutput :: Maybe String -> String -> IO ()
genOutput (Just changes) time = do
  file <- openFile "timecodes.md" AppendMode
  hPutStrLn file $ time ++ "\n " ++ changes
genOutput Nothing _ = return()

-- Returning fetched url as a string separated by \n
fetchFile :: String -> IO [String]
fetchFile url = do
    html <- simpleHttp url
    return $ (splitOn ("\n")) . toString $ html
  
-- Main Loop
timecodeGenerator :: GeneratorState -> [String] -> UTCTime -> String -> IO ()
timecodeGenerator IsFinished _ _  _ = return ()
timecodeGenerator IsRunning prevFile time url = do
    newFile <- fetchFile url
    currTime <- getCurrentTime
    let changes =  getChanges newFile prevFile
    let diffTime =  myFormatDiffTime (currTime, time)

    genOutput changes diffTime
    -- Waiting for 1 second
    threadDelay 10000
    -- Creating a loop until `IsFinished`
    if showRunning newFile == IsRunning then
      timecodeGenerator IsRunning newFile time url
    else
      timecodeGenerator IsFinished newFile time url
  
main :: IO ()
main = do
  let url = "https://hd.socks.town/s/h0jnEJQWy/download"
  time <- getCurrentTime
  file <- fetchFile url
  timecodeGenerator IsRunning file time url
