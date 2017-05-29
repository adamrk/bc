module BC.Prompt (startPrompt) where

import Control.Monad (unless)
import Data.Char
import Data.List
import System.Directory (doesFileExist, getHomeDirectory)
import System.IO
import System.Posix.Signals

import qualified System.IO.Strict as S

import BC.Config
import BC.Eval
import BC.Parse
import BC.State
import BC.Types


data Prompt = PState Int [String]


newPrompt :: IO Prompt
newPrompt = do
    home <- getHomeDirectory
    let f = home ++ "/" ++ historyFile
    exists <- doesFileExist f
    if exists
      then do
        contents <- S.readFile f
        let s = lines contents
        length s `seq` return $ PState (-1) s
      else return $ PState (-1) []


savePrompt :: Prompt -> IO ()
savePrompt (PState _ strs) = do
    home <- getHomeDirectory
    writeFile (home ++ "/" ++ historyFile) (intercalate "\n" $ reverse strs)


printHeader :: IO ()
printHeader = do
    putStrLn $ "bc (better calculator) version " ++ versionStr
    putStrLn "Copyright 2017 Veit Heller"
    putStrLn "This is free software with ABSOLUTELY NO WARRANTY.\n"


output :: State -> String -> IO State
output state out =
    let res = parse out
    in if length res == 1 && isErr (head res)
      then do
        cleanPrompt
        print (head res)
        return state
      else
        let (ret, newstate) = eval state res
        in do cleanPrompt
              putStrLn $ returnStr ++ show ret
              return newstate


printStatus :: State -> String -> IO ()
printStatus state str =
    let res = parse str
    in unless (length res == 1 && isErr (head res)) $
        let (evald, _) = eval state res
            out = show evald
        in unless (isErr evald || null out) $
          let str = " \x1b[33m=> " ++ trunc out ++ "\x1b[0m"
          in do putStr str
                moveCursor (length str - 9)
  where trunc s =
          let tr = truncLen s
          in if contains tr '\n'
              then takeWhile (/= '\n') tr ++ "..."
              else tr
        truncLen s = if length s > 20 then take 20 s ++ "..." else s


printCompletions :: State -> String -> IO ()
printCompletions state str =
  let tokens = words str
      completions = if tokens == []
                    then []
                    else getCompletions (last tokens) state
      compString = intercalate "  " $ take 5 completions
      toPrint = "\n\x1b[32m" -- color to green
              ++ replicate (length promptStr) ' '
              ++ compString
              ++ "\x1b[0m\r\x1b[A" -- color to white, move to prev line
  in do putStr toPrint
        putStr $ concat $ replicate (length promptStr + length str) "\x1b[C"


cleanPrompt :: IO ()
cleanPrompt = putStr "\x1b[2K\r"


moveCursor :: Int -> IO ()
moveCursor n = putStr (repeatB n)
  where repeatB 0 = ""
        repeatB n = '\b':repeatB (n-1)


readSpecialKey :: Int -> String -> Prompt -> IO (Int, String, Prompt)
readSpecialKey pos acc pstate@(PState hpos history) = do
    c <- getChar
    c2 <- getChar
    if c == '['
      then
        case c2 of
          'A' ->
            if hpos < (length history - 1)
              then
                let nacc = history !! (hpos+1)
                in return (length nacc, nacc, PState (hpos+1) history)
              else return (pos, acc, pstate)
          'B' ->
            if hpos > -1
              then if hpos == 0
                then return (0, "", PState (-1) history)
                else
                  let nacc = history !! (hpos-1)
                  in return (length nacc, nacc, PState (hpos-1) history)
              else return (pos, acc, pstate)
          'C' -> return (pos+1, acc, pstate)
          'D' -> return (pos-1, acc, pstate)
          _   -> return (pos, acc, pstate)
      else return (pos, acc, pstate)


readline :: State -> Prompt -> IO (Maybe String)
readline state = read' "" 0
  where read' acc pos pstate = do
          cleanPrompt
          putStr promptStr
          putStr acc
          printStatus state acc
          moveCursor (length acc - pos)
          c <- getChar
          case c of
            '\EOT' -> return Nothing
            '\n'   -> do putStrLn ""
                         return (Just acc)
            '\DEL' ->
              if null acc || pos == 0
              then read' acc pos pstate
              else do
                putStr "\b \b"
                read' (take (pos-1) acc ++ drop pos acc) (pos-1) pstate
            '\x1b' -> do
              (pos, nacc, newpstate) <- readSpecialKey pos acc pstate
              read' nacc (clamp pos 0 (length acc)) newpstate
            '\t' -> do
              printCompletions state acc
              read' acc pos pstate
            c      ->
              if isPrint c
                then do
                  putStr [c]
                  read' (take pos acc ++ [c] ++ drop pos acc) (pos+1) pstate
                else read' acc pos pstate
        clamp n min max
            | n < min = min
            | n > max = max
            | otherwise = n


prompt :: State -> Prompt -> IO Prompt
prompt state pstate@(PState _ history) = do
    input <- readline state pstate
    case input of
      Nothing -> return pstate
      Just "quit" -> return pstate
      Just str -> do
        newstate <- output state str
        let newpstate = PState (-1) (str:history)
        prompt newstate newpstate


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
    p <- newPrompt
    state <- prompt newState p
    savePrompt state
    putStrLn "Bye!"
