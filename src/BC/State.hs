module BC.State where

import Data.HashMap

import BC.Primitives
import BC.Types

type State = Map String Value

newState :: State
newState = fromList primitives

getCompletions :: String -> State -> [String]
getCompletions s = foldWithKey addKey []
  where addKey k _ ks = if isPrefix s k then k:ks else ks
        isPrefix s1 s2 = length s2 >= length s1 && (all id $ zipWith (==) s1 s2) 