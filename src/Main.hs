module Main where

import Data.Char
import Control.Concurrent
import System.Environment   
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit
import Data.List.Split
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.ByteString.Lazy.UTF8 (toString) 


data GeneratorState = IsRunning | IsFinished
  deriving (Eq)

isChanged :: Diff f -> Bool
isChanged (First _) = True
isChanged _ = False

getChanges :: [String] -> [String] -> Maybe String
getChanges now before = if (hasDiff ch) then Just res else Nothing
  where ch = filter isChanged diff
        diff = getDiff now before
        res = foldr (\(First x) y -> x ++ " \n " ++ y) "" ch

-- Should stop the script if there's a checkbox with a keyword `stop`
showRunning :: [String] -> GeneratorState
showRunning html = IsRunning

myFormatDiffTime :: (UTCTime, UTCTime) -> String
myFormatDiffTime (a,b)= formatTime defaultTimeLocale "%H:%M:%S" . posixSecondsToUTCTime $ diffUTCTime  a b

hasDiff :: [a] -> Bool
hasDiff [] = False
hasDiff _ = True


genOutput :: Maybe String -> String -> IO ()
genOutput (Just changes) time = do
  putStrLn $ time ++ "\n " ++ changes
genOutput Nothing _ = return()


timecodeGenerator :: GeneratorState -> [String] -> UTCTime -> IO ()
timecodeGenerator IsFinished _ _  = return ()
timecodeGenerator IsRunning prevFile time = do
    html <- simpleHttp "https://hd.socks.town/s/h0jnEJQWy/download"
    currTime <- getCurrentTime
    let newFile =  (splitOn ("\n")) . toString $ html
    let changes =  getChanges newFile prevFile
    let diffTime =  myFormatDiffTime (currTime, time)
    genOutput changes diffTime
    -- Waiting for 1 second
    threadDelay 10000
    -- Creating a loop until `IsFinished`
    if showRunning newFile == IsRunning then
      timecodeGenerator IsRunning newFile time
    else
      timecodeGenerator IsFinished newFile time
  
main :: IO ()
main = do
  time <- getCurrentTime
  timecodeGenerator IsRunning [""] time
