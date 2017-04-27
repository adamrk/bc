module BC.Prompt (startPrompt) where

import BC.Config

printHeader :: IO ()
printHeader = do
  putStrLn "bc (better calculator) version " ++ versionStr
  putStrLn "Copyright 2017 Veit Heller"
  putStrLn "This is free software with ABSOLUTELY NO WARRANTY.\n"


prompt :: IO ()
prompt = runInputT settings $ poll prompt
  where poll p = do
        input <- getInputLine p
        case input of
          Nothing -> return ""
          Just str -> return str


startPrompt :: IO ()
startPrompt = do
  printHeader
  prompt
