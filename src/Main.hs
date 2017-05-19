module Main where

import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdInput)

import BC.Parse
import BC.Eval
import BC.Prompt
import BC.State


main :: IO ()
main = do
    tty <- queryTerminal stdInput
    if tty
      then startPrompt
      else getContents >>= putStrLn . show . evalOne . parse
  where evalOne inp = let (val, _) = eval newState inp in val
