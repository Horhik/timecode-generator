module Main where

import Data.Char
import Control.Concurrent
import System.Environment   
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit 
import Network.HTTP.Simple (getResponseStatusCode, getResponseBody) 
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

-- TODO: Should return signal to stop the script if there's a checkbox with a keyword `stop`
showRunning :: [String] -> GeneratorState
showRunning html = IsRunning

-- Printing time diff in %H:%M:%S format
myFormatDiffTime :: (UTCTime, UTCTime) -> String
myFormatDiffTime (a,b)= formatTime defaultTimeLocale "%H:%M:%S" . posixSecondsToUTCTime $ diffUTCTime  a b

appendToFile :: String -> String -> IO()
appendToFile filename str = do
  file <- openFile filename AppendMode
  hPutStrLn file str  
  hClose file

-- Printing changes is they exsists
genOutput :: Maybe String -> String -> String -> IO ()
genOutput (Just changes) time timecodeFile= do
  appendToFile timecodeFile $ time ++ "\n " ++ changes
genOutput Nothing _ _ = return()

-- Returning fetched url as a string separated by \n
fetchFile :: String -> IO (Either String [String])
fetchFile url = do
    request <- parseRequest url
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    let statusCode = getResponseStatusCode response
    if statusCode `div` 100 == 2 then do
      return $ Right ((splitOn ("\n")) . toString $ getResponseBody response)
    else do
      return $ Left (show statusCode)
    
  
-- Main Loop
timecodeGenerator :: GeneratorState -> [String] -> UTCTime -> String -> String -> IO ()
timecodeGenerator IsFinished _ _  _ _ = return ()
timecodeGenerator IsRunning prevFile time url timecodeFile = do
    fetched <- fetchFile url
    case fetched of
      Right newFile -> do
        currTime <- getCurrentTime
        let changes =  getChanges newFile prevFile
        let diffTime =  myFormatDiffTime (currTime, time)

        genOutput changes diffTime timecodeFile 
        -- Waiting for 1 second
        threadDelay 10000
        -- Creating a loop until `IsFinished`
        if showRunning newFile == IsRunning then
          timecodeGenerator IsRunning newFile time url timecodeFile
        else
          timecodeGenerator IsFinished newFile time url timecodeFile
      Left err -> do
        print err
        timecodeGenerator IsRunning prevFile time url timecodeFile
        
-- Commandline arguments:
-- 1. Link to markdown file
-- 2. File to write timecodes to
main :: IO ()
main = do
  args <- getArgs
  let url:timecodeFile:xs = args
  time <- getCurrentTime
  file <- fetchFile url
  case file of
    Left e -> do
      putStrLn $ (++) "Error: " $ show e 
    Right file -> do
      appendToFile timecodeFile $ "New Timecodes starts here \n "
      appendToFile timecodeFile $ show time
      timecodeGenerator IsRunning file time url timecodeFile

  --let url = "https://hd.socks.town/s/h0jnEJQWy/download"
  --let timecodeFile = "timecodes.md"
