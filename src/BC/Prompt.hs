module BC.Prompt (startPrompt) where

import Data.Char
import System.IO
import System.Posix.Signals

import BC.Config
import BC.Eval
import BC.Parse
import BC.State
import BC.Types

printHeader :: IO ()
printHeader = do
    putStrLn $ "bc (better calculator) version " ++ versionStr
    putStrLn "Copyright 2017 Veit Heller"
    putStrLn "This is free software with ABSOLUTELY NO WARRANTY.\n"


output :: State -> String -> IO State
output state out =
    let res = parse out
    in if length res == 1 && isErr (res !! 0)
      then do
        putStrLn $ show (res !! 0)
        return $ state
      else
        let (ret, newstate) = eval state res
        in do putStrLn $ returnStr ++ show ret
              return newstate


printStatus :: State -> String -> IO ()
printStatus state str =
    let res = parse str
    in if length res == 1 && isErr (res !! 0)
      then return ()
      else
        let (evald, _) = eval state res
            out = show evald
        in if isErr evald || length out == 0
          then return ()
          else let str = " \x1b[33m=> " ++ trunc out ++ "\x1b[0m"
               in putStr (str ++ repeat '\b' (length str - 9))
  where repeat str 0 = ""
        repeat str n = (str:repeat str (n-1))
        contains c [] = False
        contains c (s:xs) = if c == s then True else contains c xs
        trunc s =
          let tr = truncLen s
          in if contains '\n' tr
              then takeWhile (\x -> x /= '\n') tr ++ "..."
              else tr
        truncLen s = if length s > 20 then take 20 s ++ "..." else s


cleanPrompt :: IO ()
cleanPrompt = putStr "\x1b[2K\r"


-- TODO: Stub
readSpecialKey :: IO ()
readSpecialKey = do
    c <- getChar
    c2 <- getChar
    return ()


readline :: State -> IO (Maybe String)
readline state = read' ""
  where read' acc = do
          cleanPrompt
          putStr promptStr
          putStr acc
          printStatus state acc
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
            '\x1b' -> do
              readSpecialKey
              read' acc
            c    ->
              if isPrint c
                then do
                  putStr [c]
                  read' (acc ++ [c])
                else read' acc


prompt :: State -> IO ()
prompt state = do
    input <- readline state
    case input of
      Nothing -> putStrLn "\nBye!"
      Just "quit" -> putStrLn "\nBye!"
      Just str -> do
        putStrLn ""
        newstate <- output state str
        prompt newstate


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
    prompt newState
