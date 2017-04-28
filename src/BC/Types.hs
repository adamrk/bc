module BC.Types where

data Value = BInt Integer
           | BOp String
           | BErr String
instance Show Value where
  show (BInt i) = show i
  show (BOp  o) = o
  show (BErr e) = "error: " ++ e

isErr :: Value -> Bool
isErr (BErr _) = True
isErr _        = False
