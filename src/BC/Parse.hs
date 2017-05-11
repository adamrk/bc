module BC.Parse (parse) where

import Data.Char

import qualified Text.ParserCombinators.Parsec as P

import BC.Types

symbol :: P.Parser Char
symbol = P.oneOf "!%&|*+-/<=>^~"


number :: P.Parser Value
number = P.try float P.<|> integer


float :: P.Parser Value
float = do
    neg <- P.optionMaybe (P.string "-")
    x <- P.many1 P.digit
    _ <- P.string "."
    y <- P.many1 P.digit
    case neg of
      Just "-" -> (return . BFloat . read) ("-" ++ x ++ "." ++ y)
      _        -> (return . BFloat . read) (x ++ "." ++ y)


integer :: P.Parser Value
integer = do
    neg <- P.optionMaybe (P.string "-")
    x <- P.many1 P.digit
    case neg of
      Just "-" -> (return . BInt . read) ("-" ++ x)
      _        -> (return . BInt . read) x


bool :: P.Parser Value
bool = P.try parseTrue P.<|> parseFalse
    where parseTrue = do
            _ <- P.string "true"
            return $ BBool True
          parseFalse = do
            _ <- P.string "false"
            return $ BBool False


operator :: P.Parser Value
operator = do
    res <- P.many1 $ P.letter P.<|> symbol
    return $ BOp res


types :: P.Parser Value
types = P.try bool P.<|> P.try operator P.<|> number


parser :: P.Parser [Value]
parser = (P.sepBy types P.spaces) <* P.eof

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
