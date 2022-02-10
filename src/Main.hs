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
import qualified Data.ByteString.Lazy.Char8 as L8


data GeneratorState = IsRunning | IsFinished
  deriving (Eq)

getChanges :: Diff f -> Bool
getChanges (First _) = True
getChanges _ = False

-- Should stop the script if there's a checkbox with a keyword `stop`
showRunning :: [String] -> GeneratorState
showRunning html = IsRunning

myFormatDiffTime :: (UTCTime, UTCTime) -> String
myFormatDiffTime (a,b)= formatTime defaultTimeLocale "%H:%M:%S" . posixSecondsToUTCTime $ diffUTCTime  a b

hasDiff :: [a] -> Bool
hasDiff [] = False
hasDiff _ = True

timecodeGenerator :: GeneratorState -> [String] -> UTCTime -> IO ()
timecodeGenerator IsFinished _ _  = return ()
timecodeGenerator IsRunning text time = do
    html <- simpleHttp "https://hd.socks.town/s/h0jnEJQWy/download"
    currTime <- getCurrentTime
    let body =  (splitOn ("\n")) . L8.unpack $ html
    print $ filter getChanges $ getDiff body text
    print $ myFormatDiffTime (currTime, time)
    -- Waiting for 1 second
    threadDelay 1000000
    -- Creating a loop until `IsFinished`
    if showRunning body == IsRunning then
      timecodeGenerator IsRunning body time
    else
      timecodeGenerator IsFinished body time
  
main :: IO ()
main = do
  time <- getCurrentTime
  timecodeGenerator IsRunning [""] time
