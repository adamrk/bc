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


printStatus :: String -> IO ()
printStatus str =
    let res = parse str
    in if length res == 1 && isErr (res !! 0)
      then return ()
      else
        let evald = eval res
            out = show evald
        in if isErr evald || length out == 0
          then return ()
          else let str = " \x1b[33m=> " ++ out ++ "\x1b[0m"
               in putStr (str ++ repeat '\b' (length str - 9))
  where repeat str 0 = ""
        repeat str n = (str:repeat str (n-1))


readline :: IO (Maybe String)
readline = do
    putStr promptStr
    read' ""
  where read' acc = do
          printStatus acc
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
      Nothing -> putStrLn "\nBye!"
      Just "quit" -> putStrLn "\nBye!"
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
