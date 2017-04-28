module BC.Eval (eval) where

import BC.Types

eval :: [Value] -> Value
eval [x@(BInt _)] = x
eval [x@(BErr _)] = x
eval [(BOp  x)] = BErr ("operation " ++ x ++ " requires arguments")
eval [] = BOp ""
eval l = treeEval l [] []

treeEval :: [Value] -> [Value] -> [Value] -> Value
treeEval [] [] (num:_) = num
treeEval [] ops nums = handleOp [] ops nums
treeEval (x@(BInt _):xy) ops nums = treeEval xy ops (nums ++ [x])
treeEval expr@(x@(BOp _):xy) ops@(op:_) nums =
    if precedence x > precedence op
      then treeEval xy (x:ops) nums
      else handleOp expr ops nums
treeEval (x@(BOp _):xy) [] nums = treeEval xy [x] nums

handleOp :: [Value] -> [Value] -> [Value] -> Value
handleOp expr (op:ops) (op1:(op2:nums)) =
    treeEval expr ops ((evalOp op op1 op2):nums)

evalOp :: Value -> Value -> Value -> Value
evalOp (BOp "*") (BInt x) (BInt y) = BInt $ x * y
evalOp (BOp "/") (BInt x) (BInt y) = BInt $ quot x y
evalOp (BOp "+") (BInt x) (BInt y) = BInt $ x + y
evalOp (BOp "-") (BInt x) (BInt y) = BInt $ x - y
evalOp (BOp "^") (BInt x) (BInt y) = BInt $ x ^ y
evalOp _ _ _ = BInt 0
