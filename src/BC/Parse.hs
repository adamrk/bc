module BC.Parse (parse) where

import Data.Char

import qualified Text.ParserCombinators.Parsec as P

import BC.Types

optspace :: P.Parser (Maybe ())
optspace = P.optionMaybe P.spaces


symchar :: P.Parser Char
symchar = P.oneOf "!%&|*+-/<=>^~"


number :: P.Parser Value
number = P.try float P.<|> integer


commasep parser = P.sepBy parser sep
  where sep = do _ <- optspace
                 str <- P.string ","
                 _ <- optspace
                 return str


float :: P.Parser Value
float = do
    neg <- P.optionMaybe (P.string "-")
    x <- P.many1 P.digit
    _ <- P.string "."
    y <- P.many1 P.digit
    case neg of
      Just "-" -> return $ construct ("-" ++ x ++ "." ++ y)
      _        -> return $ construct (x ++ "." ++ y)
        -- do a little dance
  where construct str = BNum $ BFloat $ fromRational $ realToFrac (read str::Double)


integer :: P.Parser Value
integer = do
    neg <- P.optionMaybe (P.string "-")
    x <- P.many1 P.digit
    case neg of
      Just "-" -> (return . BNum . BInt . read) ("-" ++ x)
      _        -> (return . BNum . BInt . read) x


bool :: P.Parser Value
bool = P.try parseTrue P.<|> parseFalse
    where parseTrue = do
            _ <- P.string "true"
            return $ BBool True
          parseFalse = do
            _ <- P.string "false"
            return $ BBool False


symbol :: P.Parser Value
symbol = do
    res <- P.many1 $ P.letter P.<|> symchar
    if isKeyword res
      then P.unexpected res
      else return $ BSym res


block :: P.Parser [[Value]]
block = do
    _ <- P.string "{"
    _ <- optspace
    body <- P.sepBy parser newline
    _ <- optspace
    _ <- P.string "}"
    return body
  where newline = do
          _ <- P.many (P.string " " P.<|> P.string "\t")
          _ <- P.string "\n"
          P.many (P.string " " P.<|> P.string "\t")


headAndBlock parser = do
    _ <- optspace
    _ <- P.string "("
    _ <- optspace
    args <- parser
    _ <- optspace
    _ <- P.string ")"
    _ <- optspace
    body <- block
    return (args, body)


parseIf :: P.Parser Value
parseIf = do
    _ <- P.string "if"
    (cond, body) <- headAndBlock parser
    alt <- P.optionMaybe (P.string "else")
    case alt of
      Just _ -> do
        _ <- optspace
        altbody <- block
        return $ BIf cond body (Just altbody)
      Nothing -> return $ BIf cond body Nothing


while :: P.Parser Value
while = do
    _ <- P.string "while"
    (cond, body) <- headAndBlock parser
    return $ BWhile cond body


def :: P.Parser Value
def = do
    sym <- symbol
    _ <- P.spaces
    _ <- P.string "="
    _ <- optspace
    expr <- parser
    return $ BDef sym expr


fun :: P.Parser Value
fun = do
    _ <- P.string "define"
    _ <- P.spaces
    name <- P.many1 $ P.letter P.<|> symchar
    (args, body) <- headAndBlock (commasep (P.many1 $ P.letter P.<|> symchar))
    return $ BFun name args body


braces :: P.Parser Value
braces = do
       _ <- P.string "("
       parsed <- outerparser
       _ <- P.string ")"
       return $ BBraced parsed


call :: P.Parser Value
call = do
    name <- symbol
    _ <- P.string "("
    _ <- optspace
    args <- commasep parser
    _ <- optspace
    _ <- P.string ")"
    return $ BCall name args



expr :: P.Parser Value
expr = P.try bool
 P.<|> P.try def
 P.<|> P.try while
 P.<|> P.try parseIf
 P.<|> P.try fun
 P.<|> P.try braces
 P.<|> P.try call
 P.<|> P.try number
 P.<|> symbol


parser :: P.Parser [Value]
parser = P.sepBy expr (P.string " " P.<|> P.string "\t")


outerparser :: P.Parser [Value]
outerparser = P.sepBy expr P.spaces


parse :: String -> [Value]
parse input = case P.parse (outerparser <* P.eof) (trim input) (trim input) of
    Left err  -> [BErr $ show err]
    Right val -> val
  where trim s = trimR "" $ dropWhile isSpace s
        trimR s "" = ""
        trimR s (x:xs)
          | isSpace x = trimR (x:s) xs
          | null s    = x:trimR "" xs
          | otherwise = reverse s ++ x:trimR "" xs
