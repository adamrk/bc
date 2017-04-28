module BC.Prompt (startPrompt) where

import System.Console.Haskeline
import System.Posix.Signals

import BC.Config

printHeader :: IO ()
printHeader = do
  putStrLn $ "bc (better calculator) version " ++ versionStr
  putStrLn "Copyright 2017 Veit Heller"
  putStrLn "This is free software with ABSOLUTELY NO WARRANTY.\n"


output :: [Char] -> InputT IO ()
output out = outputStrLn $ returnStr ++ out


prompt :: IO ()
prompt = runInputT defaultSettings $ poll promptStr
  where poll p = do
          input <- getInputLine p
          case input of
            Nothing -> do
              output "Bye!"
              return ()
            Just "quit" -> do
              output "Bye!"
              return ()
            Just str -> do
              output str
              poll p


installHandlers :: IO ()
installHandlers = do
  installHandler keyboardSignal (Catch (putStrLn "\n(interrupt) type quit to exit")) Nothing
  return ()


startPrompt :: IO ()
startPrompt = do
  printHeader
  installHandlers
  prompt
