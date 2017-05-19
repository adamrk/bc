module BC.Parse (parse) where

import Data.Char

import qualified Text.ParserCombinators.Parsec as P

import BC.Types

symchar :: P.Parser Char
symchar = P.oneOf "!%&|*+-/<=>^~"


number :: P.Parser Value
number = P.try float P.<|> integer


float :: P.Parser Value
float = do
    neg <- P.optionMaybe (P.string "-")
    x <- P.many1 P.digit
    _ <- P.string "."
    y <- P.many1 P.digit
    case neg of
      Just "-" -> (return . BNum . BFloat . read) ("-" ++ x ++ "." ++ y)
      _        -> (return . BNum . BFloat . read) (x ++ "." ++ y)


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
    return $ BSym res


block :: P.Parser [Value]
block = do
    _ <- P.string "{"
    _ <- P.optionMaybe P.spaces
    body <- P.sepBy expr P.spaces
    _ <- P.optionMaybe P.spaces
    _ <- P.string "}"
    return $ body


-- I obviously can't parsec
parseIf :: P.Parser Value
parseIf = do
    _ <- P.string "if"
    _ <- P.optionMaybe P.spaces
    _ <- P.string "("
    _ <- P.optionMaybe P.spaces
    cond <- P.sepBy expr P.spaces
    _ <- P.optionMaybe P.spaces
    _ <- P.string ")"
    _ <- P.optionMaybe P.spaces
    body <- block
    _ <- P.optionMaybe P.spaces
    alt <- P.optionMaybe (P.string "else")
    case alt of
      Just _ -> do
        _ <- P.optionMaybe P.spaces
        altbody <- block
        return $ BIf cond body (Just altbody)
      Nothing -> return $ BIf cond body Nothing


while :: P.Parser Value
while = do
    _ <- P.string "while"
    _ <- P.optionMaybe P.spaces
    _ <- P.string "("
    _ <- P.optionMaybe P.spaces
    cond <- P.sepBy expr P.spaces
    _ <- P.optionMaybe P.spaces
    _ <- P.string ")"
    _ <- P.optionMaybe P.spaces
    body <- block
    _ <- P.optionMaybe P.spaces
    return $ BWhile cond body


def :: P.Parser Value
def = do
    sym <- symbol
    _ <- P.spaces
    _ <- P.string "="
    _ <- P.optionMaybe P.spaces
    expr <- P.sepBy expr P.spaces
    return $ BDef sym expr


expr :: P.Parser Value
expr = P.try bool
 P.<|> P.try def
 P.<|> P.try while
 P.<|> P.try parseIf
 P.<|> P.try number
 P.<|> symbol


parser :: P.Parser [Value]
parser = (P.sepBy expr P.spaces) <* P.eof

parse :: String -> [Value]
parse input = case P.parse parser (trim input) (trim input) of
    Left err  -> [BErr $ show err]
    Right val -> val
  where trim s = trimR "" $ dropWhile isSpace s
        trimR s "" = ""
        trimR s (x:xs)
          | isSpace x = trimR (x:s) xs
          | null s    = x:trimR "" xs
          | otherwise = reverse s ++ x:trimR "" xs
