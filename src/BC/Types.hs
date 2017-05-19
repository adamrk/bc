module BC.Types where

import Data.List (intercalate)

data Value = BNum Number
           | BBool Bool
           | BSym String
           | BIf [Value] [[Value]] (Maybe [[Value]])
           | BWhile [Value] [[Value]]
           | BDef Value [Value]
           | BFun String [String] [[Value]]
           | BCall Value [[Value]]
           | BErr String
instance Show Value where
  show (BBool b) = if b then "true" else "false"
  show (BDef sym expr) = show sym ++ " = " ++ unwords (map show expr)
  show (BIf cond body alt) =
    "if (" ++ unwords (map show cond) ++ ") {\n  " ++
    unwords (map show body) ++ "\n}" ++
      (case alt of
        Just vals ->
          " else {\n  " ++ unwords (map show vals) ++ "\n}"
        Nothing -> "")
  show (BWhile cond body) =
    "while (" ++ unwords (map show cond) ++ ") {\n  " ++
    unwords (map show body) ++ "\n}"
  show (BFun name args body) =
    "define " ++ name ++ "(" ++ intercalate ", " args ++ ") {\n  " ++
    unwords (map show body) ++ "\n}"
  show (BCall name args) =
    show name ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (BSym  o) = o
  show (BNum n) = show n
  show (BErr e) = "error: " ++ e


-- sorry, this is a little hacky
data Number = BInt Integer
            | BFloat Double
  deriving (Ord, Eq)
instance Show Number where
  show (BInt i) = show i
  show (BFloat f) = show f

instance Fractional Number where
  (BInt x) / (BInt y) = BInt $ quot x y
  (BFloat x) / (BFloat y) = BFloat $ x / y
  (BInt x) / (BFloat y) = BFloat $ fromIntegral x / y
  (BFloat x) / (BInt y) = BFloat $ x / fromIntegral y

instance Floating Number where
    (BFloat x) ** (BFloat y) = BFloat $ x ** y
    (BFloat x) ** (BInt y) = BFloat $ x ** fromIntegral y
    (BInt x) ** (BFloat y) = BFloat $ fromIntegral x ** y
    (BInt x) ** (BInt y) = BInt $ x ^ y
    logBase x y =  log y / log x
    sqrt x = x ** BFloat 0.5
    tan  x = sin x / cos x
    tanh x =  sinh x / cosh x

instance Num Number where
  (BInt x) + (BInt y) = BInt $ x + y
  (BFloat x) + (BFloat y) = BFloat $ x + y
  (BInt x) + (BFloat y) = BFloat $ fromIntegral x + y
  (BFloat x) + (BInt y) = BFloat $ x + fromIntegral y
  (BInt x) * (BInt y) = BInt $ x * y
  (BFloat x) * (BFloat y) = BFloat $ x * y
  (BInt x) * (BFloat y) = BFloat $ fromIntegral x * y
  (BFloat x) * (BInt y) = BFloat $ x * fromIntegral y
  abs (BInt x) = BInt $ abs x
  abs (BFloat x) = BFloat $ abs x
  signum (BInt x) = BInt $ signum x
  signum (BFloat x) = BFloat $ signum x
  fromInteger = BInt
  negate (BInt x) = BInt $ negate x
  negate (BFloat x) = BFloat $ negate x


isErr :: Value -> Bool
isErr (BErr _) = True
isErr _        = False


precedence :: String -> Int
precedence "^" = 5
precedence "*" = 4
precedence "/" = 4
precedence "-" = 3
precedence "+" = 3
precedence "%" = 4
precedence "||" = 1
precedence "&&" = 1
precedence "<" = 2
precedence ">" = 2
precedence "<=" = 2
precedence ">=" = 2
precedence "==" = 2
precedence "!=" = 2


contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xy) y = (x == y) || contains xy y


operators = ["<=", ">=", "==", "!=", "<", ">", "||", "&&", "^", "*", "/", "-", "+", "%"]


isOp = contains operators


keywords = ["define", "if", "else", "while"]


isKeyword = contains keywords
