module BC.Parse (parse) where

import qualified Text.ParserCombinators.Parsec as P

import BC.Types

symbol :: P.Parser Char
symbol = P.oneOf "!%&|*+-/<=>^~"

number :: P.Parser Value
number = do
    neg <- P.optionMaybe (P.string "-" P.<|> P.string "+")
    x <- P.many1 P.digit
    case neg of
      Just "-" -> (return . BInt . read) ("-" ++ x)
      _        -> (return . BInt . read) x

operator :: P.Parser Value
operator = do
    res <- P.many1 $ P.letter P.<|> symbol
    return $ BOp res

parser :: P.Parser [Value]
parser = (P.sepBy (P.try operator P.<|> number) P.spaces) <* P.eof

parse :: String -> [Value]
parse input = case P.parse parser input input of
    Left err  -> [BErr $ show err]
    Right val -> val
