module BC.Eval (eval) where

import qualified Data.HashMap as M

import BC.State
import BC.Types

truthy :: Value -> Bool
truthy (BNum num) = num /= 0
truthy (BBool x) = x
truthy _ = False


evalAll :: State -> [[Value]] -> (Value, State)
evalAll state [x] = eval state x
evalAll state (x:xs) =
  let (_, nstate) = eval state x
  in evalAll nstate xs


eval :: State -> [Value] -> (Value, State)
eval state [x@(BNum _)] = (x, state)
eval state [BDef (BSym sym) expr] =
    let (val, newstate) = eval state expr
    in (val, M.insert sym val newstate)
eval state [x@(BFun sym _ _)] = (BBool True, M.insert sym x state)
eval state [x@(BBool _)] = (x, state)
eval state [x@(BErr _)] = (x, state)
eval state [x@(BWhile cond body)] =
    let (evald, whilestate) = eval state cond
    in
      if truthy evald
        then let
          (bodyval, newstate) = evalAll whilestate body
          (val, retstate) = eval newstate [x]
          in if truthy val then (val, retstate) else (bodyval, retstate)
        else (BBool False, whilestate)
eval state [BIf cond body alt] =
    let (evald, ifstate) = eval state cond
    in
      if truthy evald
        then evalAll ifstate body
        else case alt of
              Just vals -> evalAll ifstate vals
              Nothing -> (BBool False, ifstate)
eval state [BSym x] =
    case M.lookup x state of
      Just val -> (val, state)
      Nothing  -> (BErr (x ++ " is undefined"), state)
eval state [BCall (BSym name) args] =
    case M.lookup name state of
      Just val@BFun{} -> funCall state val args
      Nothing -> (BErr ("function " ++ name ++ " is undefined"), state)
      _ -> (BErr (name ++ " is not a function"), state)
eval state [] = (BSym "", state)
eval state l = (treeEval state l [] [], state)


treeEval :: State -> [Value] -> [Value] -> [Value] -> Value
treeEval _ [] [] (num:_) = num
treeEval state [] ops nums = handleOp state [] ops nums
treeEval state (x@(BNum _):xy) ops nums = treeEval state xy ops (x:nums)
treeEval state (BBool x:xy) ops nums =
    treeEval state xy ops ((BNum $ BInt $ if x then 1 else 0):nums)
treeEval state expr@(x@(BSym sym):xy) ops@(BSym op:_) nums =
    case M.lookup sym state of
      Just val -> treeEval state xy ops (val:nums)
      Nothing  ->
        if precedence sym > precedence op
          then treeEval state xy (x:ops) nums
          else handleOp state expr ops nums
treeEval state (x@(BSym sym):xy) [] nums =
    case M.lookup sym state of
      Just val -> treeEval state xy [] (val:nums)
      Nothing  ->
        if isOp sym
          then treeEval state xy [x] nums
          else BErr (sym ++ " is undefined")
treeEval state (x:xy) ops vals =
    let (val, nstate) = eval state [x]
    in treeEval nstate (val:xy) ops vals


handleOp :: State -> [Value] -> [Value] -> [Value] -> Value
handleOp state expr (BSym op:ops) (BNum op2:(BNum op1:nums)) =
    treeEval state expr ops (findOp op op1 op2:nums)
handleOp _ expr (BSym op:ops) x = BErr ("Not enough arguments to operation " ++ op)


findOp x = case binOp x of
            Just op -> \a b -> (BNum $ op a b)
            Nothing ->
              case logicalOp x of
                Just lop -> \a b -> BBool $ lop a b
                Nothing -> \a b -> BNum $ BInt 0


logicalOp :: String -> Maybe (Number -> Number -> Bool)
logicalOp ">" = Just (>)
logicalOp "<" = Just (<)
logicalOp ">=" = Just (>=)
logicalOp "<=" = Just (<=)
logicalOp "==" = Just (==)
logicalOp "!=" = Just (/=)
logicalOp _ = Nothing


binOp :: String -> Maybe (Number -> Number -> Number)
binOp "*" = Just (*)
binOp "/" = Just (/)
binOp "+" = Just (+)
binOp "-" = Just ( - )
binOp "^" = Just (**)
binOp _ = Nothing


funCall :: State -> Value -> [[Value]] -> (Value, State)
funCall state (BFun name args body) provided =
    if length args == length provided
      then let
        nstate = callWith state args provided
        (val, _) = evalAll nstate body
        in (val, state)
      else
        (BErr ("Expected " ++ show (length args) ++
               " arguments in call to function " ++ name ++ ", got " ++
               show (length provided)),
         state)
  where callWith state [] _ = state
        callWith state (a:args) (p:provided) =
          let (evald, nstate) = eval state p
          in callWith (M.insert a evald nstate) args provided
