module BC.Eval (eval) where

import qualified Data.HashMap as M

import BC.State
import BC.Types

truthy :: Value -> Bool
truthy (BNum num) = num /= 0
truthy (BBool x) = x
truthy _ = False


eval :: State -> [Value] -> (Value, State)
eval state [x@(BNum _)] = (x, state)
eval state [(BDef (BSym sym) expr)] =
    let (val, newstate) = eval state expr
    in (val, M.insert sym val newstate)
eval state [x@(BBool _)] = (x, state)
eval state [x@(BErr _)] = (x, state)
eval state [x@(BWhile cond body)] =
    let (evald, whilestate) = eval state cond
    in
      if truthy evald
        then let
          (bodyval, newstate) = eval whilestate body
          (val, retstate) = eval newstate [x]
          in if truthy val then (val, retstate) else (bodyval, retstate)
        else (BBool False, whilestate)
eval state [(BIf cond body alt)] =
    let (evald, ifstate) = eval state cond
    in
      if truthy evald
        then eval ifstate body
        else case alt of
              Just vals -> eval ifstate vals
              Nothing -> (BBool False, ifstate)
eval state [(BSym x)] =
    case M.lookup x state of
      Just val -> (val, state)
      Nothing  -> (BErr (x ++ " is undefined"), state)
eval state [] = (BSym "", state)
eval state l = (treeEval state l [] [], state)


treeEval :: State -> [Value] -> [Value] -> [Value] -> Value
treeEval _ [] [] (num:_) = num
treeEval state [] ops nums = handleOp state [] ops nums
treeEval state (x@(BIf _ _ _):xy) ops nums =
    let (val, newstate) = eval state [x]
    in treeEval newstate (val:xy) ops nums
treeEval state (x@(BNum _):xy) ops nums = treeEval state xy ops (x:nums)
treeEval state ((BBool x):xy) ops nums =
    treeEval state xy ops ((BNum $ BInt $ if x then 1 else 0):nums)
treeEval state expr@(x@(BSym sym):xy) ops@(op:_) nums =
    case M.lookup sym state of
      Just val -> treeEval state xy ops (val:nums)
      Nothing  ->
        if precedence x > precedence op
          then treeEval state xy (x:ops) nums
          else handleOp state expr ops nums
treeEval state (x@(BSym sym):xy) [] nums =
    case M.lookup sym state of
      Just val -> treeEval state xy [] (val:nums)
      Nothing  -> treeEval state xy [x] nums


handleOp :: State -> [Value] -> [Value] -> [Value] -> Value
handleOp state expr (op:ops) ((BNum op2):((BNum op1):nums)) =
    treeEval state expr ops (((findOp op) op1 op2):nums)
handleOp _ expr ((BSym op):ops) _ = BErr ("Not enough arguments to operation " ++ op)


findOp x = case binOp x of
            Just op -> \a -> \b -> (BNum $ op a b)
            Nothing ->
              case logicalOp x of
                Just lop -> \a -> \b -> (BBool $ lop a b)
                Nothing -> \a -> \b -> (BNum $ BInt 0)


logicalOp :: Value -> Maybe (Number -> Number -> Bool)
logicalOp (BSym ">") = Just (>)
logicalOp (BSym "<") = Just (<)
logicalOp (BSym ">=") = Just (>=)
logicalOp (BSym "<=") = Just (<=)
logicalOp (BSym "==") = Just (==)
logicalOp (BSym "!=") = Just (/=)
logicalOp _ = Nothing


binOp :: Value -> Maybe (Number -> Number -> Number)
binOp (BSym "*") = Just (*)
binOp (BSym "/") = Just (/)
binOp (BSym "+") = Just (+)
binOp (BSym "-") = Just ( - )
binOp (BSym "^") = Just (**)
binOp _ = Nothing
