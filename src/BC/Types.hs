module BC.Types where

data Value = BInt Integer
           | BFloat Double
           | BOp String
           | BErr String
instance Show Value where
  show (BInt i) = show i
  show (BFloat f) = show f
  show (BOp  o) = o
  show (BErr e) = "error: " ++ e

isErr :: Value -> Bool
isErr (BErr _) = True
isErr _        = False

precedence :: Value -> Int
precedence (BOp "^") = 3
precedence (BOp "*") = 2
precedence (BOp "/") = 2
precedence (BOp "-") = 1
precedence (BOp "+") = 1
precedence (BOp "%") = 2
precedence _         = 0
