module BC.Eval (eval) where

import BC.Types

eval :: [Value] -> Value
eval [x@(BNum _)] = x
eval [x@(BBool _)] = x
eval [x@(BErr _)] = x
eval [(BOp x)] = BErr ("operation " ++ x ++ " requires arguments")
eval [] = BOp ""
eval l = treeEval l [] []

treeEval :: [Value] -> [Value] -> [Value] -> Value
treeEval [] [] (num:_) = num
treeEval [] ops nums = handleOp [] ops nums
treeEval (x@(BNum _):xy) ops nums = treeEval xy ops (x:nums)
treeEval ((BBool x):xy) ops nums =
    treeEval xy ops ((BNum $ BInt $ if x then 1 else 0):nums)
treeEval expr@(x@(BOp _):xy) ops@(op:_) nums =
    if precedence x > precedence op
      then treeEval xy (x:ops) nums
      else handleOp expr ops nums
treeEval (x@(BOp _):xy) [] nums = treeEval xy [x] nums

handleOp :: [Value] -> [Value] -> [Value] -> Value
handleOp expr (op:ops) ((BNum op2):((BNum op1):nums)) =
    treeEval expr ops (((findOp op) op1 op2):nums)
handleOp expr ((BOp op):ops) _ = BErr ("Not enough arguments to operation " ++ op)


findOp x = case binOp x of
            Just op -> \a -> \b -> (BNum $ op a b)
            Nothing ->
              case logicalOp x of
                Just lop -> \a -> \b -> (BBool $ lop a b)
                Nothing -> \a -> \b -> (BNum $ BInt 0)


logicalOp :: Value -> Maybe (Number -> Number -> Bool)
logicalOp (BOp ">") = Just (>)
logicalOp (BOp "<") = Just (<)
logicalOp (BOp ">=") = Just (>=)
logicalOp (BOp "<=") = Just (<=)
logicalOp (BOp "==") = Just (==)
logicalOp (BOp "!=") = Just (/=)
logicalOp _ = Nothing


binOp :: Value -> Maybe (Number -> Number -> Number)
binOp (BOp "*") = Just (*)
binOp (BOp "/") = Just (/)
binOp (BOp "+") = Just (+)
binOp (BOp "-") = Just (-)
binOp (BOp "^") = Just (**)
binOp _ = Nothing
