module BC.Prompt (startPrompt) where

import Data.Char
import System.IO
import System.Posix.Signals

import BC.Config

printHeader :: IO ()
printHeader = do
    putStrLn $ "bc (better calculator) version " ++ versionStr
    putStrLn "Copyright 2017 Veit Heller"
    putStrLn "This is free software with ABSOLUTELY NO WARRANTY.\n"


output :: String -> IO ()
output out = putStrLn $ returnStr ++ out


readline :: IO (Maybe String)
readline = do
    putStr promptStr
    hFlush stdout
    read' ""
  where read' acc = do
          c <- getChar
          case c of
            '\EOT' -> return Nothing
            '\n' -> return (Just acc)
            c    -> read' (acc ++ [c])


prompt :: IO ()
prompt = do
    input <- readline
    case input of
      Nothing -> do
        output "Bye!"
        return ()
      Just "quit" -> do
        output "Bye!"
        return ()
      Just str -> do
        output str
        prompt


installHandlers :: IO ()
installHandlers = do
    installHandler keyboardSignal (Catch (putStrLn "\n(interrupt) type quit to exit")) Nothing
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    return ()


startPrompt :: IO ()
startPrompt = do
    printHeader
    installHandlers
    prompt
