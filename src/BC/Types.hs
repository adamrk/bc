module BC.Types where

data Value = BNum Number
           | BBool Bool
           | BOp String
           | BErr String
instance Show Value where
  show (BBool b) = if b then "true" else "false"
  show (BOp  o) = o
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
    sqrt x = x ** (BFloat 0.5)
    tan  x = sin x / cos x
    tanh x =  sinh x / cosh x

instance Num Number where
  (BInt x) + (BInt y) = BInt $ x * y
  (BFloat x) + (BFloat y) = BFloat $ x * y
  (BInt x) + (BFloat y) = BFloat $ fromIntegral x * y
  (BFloat x) + (BInt y) = BFloat $ x * fromIntegral y
  (BInt x) * (BInt y) = BInt $ x * y
  (BFloat x) * (BFloat y) = BFloat $ x * y
  (BInt x) * (BFloat y) = BFloat $ fromIntegral x * y
  (BFloat x) * (BInt y) = BFloat $ x * fromIntegral y
  abs (BInt x) = BInt $ abs x
  abs (BFloat x) = BFloat $ abs x
  signum (BInt x) = BInt $ signum x
  signum (BFloat x) = BFloat $ signum x
  fromInteger x = BInt x
  negate (BInt x) = BInt $ negate x
  negate (BFloat x) = BFloat $ negate x


isErr :: Value -> Bool
isErr (BErr _) = True
isErr _        = False


precedence :: Value -> Int
precedence (BOp "^") = 5
precedence (BOp "*") = 4
precedence (BOp "/") = 4
precedence (BOp "-") = 3
precedence (BOp "+") = 3
precedence (BOp "%") = 4
precedence (BOp "||") = 1
precedence (BOp "&&") = 1
precedence (BOp "<") = 2
precedence (BOp ">") = 2
precedence (BOp "<=") = 2
precedence (BOp ">=") = 2
precedence (BOp "==") = 2
precedence (BOp "!=") = 2
precedence _         = 0
