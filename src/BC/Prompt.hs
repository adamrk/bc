module BC.Prompt (startPrompt) where

import Data.Char
import System.IO
import System.Posix.Signals

import BC.Config
import BC.Eval
import BC.Parse
import BC.Types

printHeader :: IO ()
printHeader = do
    putStrLn $ "bc (better calculator) version " ++ versionStr
    putStrLn "Copyright 2017 Veit Heller"
    putStrLn "This is free software with ABSOLUTELY NO WARRANTY.\n"


output :: String -> IO ()
output out =
    let res = parse out
    in if length res == 1 && isErr (res !! 0)
      then putStrLn $ show (res !! 0)
      else putStrLn $ returnStr ++ show (eval res)


readline :: IO (Maybe String)
readline = do
    putStr promptStr
    read' ""
  where read' acc = do
          c <- getChar
          case c of
            '\EOT' -> return Nothing
            '\n' -> return (Just acc)
            '\DEL' ->
              if length acc > 0
              then do
                putStr "\b \b"
                read' (init acc)
              else do
                read' acc
            c    ->
              if isPrint c
                then do
                  putStr [c]
                  read' (acc ++ [c])
                else read' acc


prompt :: IO ()
prompt = do
    input <- readline
    case input of
      Nothing -> putStrLn "Bye!"
      Just "quit" -> putStrLn "Bye!"
      Just str -> do
        putStrLn ""
        output str
        prompt


installHandlers :: IO ()
installHandlers = do
    installHandler keyboardSignal (Catch (putStrLn "\n(interrupt) type quit to exit")) Nothing
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering


startPrompt :: IO ()
startPrompt = do
    printHeader
    installHandlers
    prompt
