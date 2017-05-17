module BC.State where

import Data.HashMap

import BC.Types

type State = Map String Value

newState :: State
newState = empty
