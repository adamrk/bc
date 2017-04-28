module BC.Eval (eval) where

import BC.Types

eval :: [Value] -> Value
eval [x@(BInt _)] = x
eval [(BOp  x)] = BErr ("operation " ++ x ++ " requires arguments")
eval (x:xy) = x
eval [] = BOp ""
