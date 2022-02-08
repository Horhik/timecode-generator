module Main where

import Data.Char
import Control.Concurrent
import System.Environment   
import Data.Time.Clock
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Conduit
import Data.List.Split
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput

data GeneratorState = IsRunning | IsFinished
  deriving (Eq)

getChanges :: Diff f -> Bool
getChanges (First _) = True
getChanges _ = False

showRunning html = IsRunning

getCheckBoxList html = html

timecodeGenerator :: GeneratorState -> [String] -> UTCTime -> IO ()
timecodeGenerator IsFinished _ _  = return ()
timecodeGenerator IsRunning text time = do
    html <- simpleHttp "https://hd.socks.town/s/h0jnEJQWy/download"
    let body =  (splitOn ("\n")) . L8.unpack $ html
    print $ filter getChanges $ getDiff body text

    threadDelay 1000000
    if showRunning body == IsRunning then
      timecodeGenerator IsRunning body time
    else
      timecodeGenerator IsFinished body time


      
  
main :: IO ()
main = do
  time <- getCurrentTime
  timecodeGenerator IsRunning [""] time
