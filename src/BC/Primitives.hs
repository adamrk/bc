module BC.Primitives where

import BC.C
import BC.Types

primitives :: [(String, Value)]
primitives = map valuize primitivesList
  where valuize (x, y) = (x, BNative y)


primitivesList :: [(String, [Value] -> Value)]
primitivesList = [ ("c", pcos)
                 , ("s", psine)
                 , ("a", parc)
                 , ("l", plog)
                 , ("e", ppow)
                 , ("j", pbessel)
                 , ("sqrt", psqrt)
                 ]


pmath :: (BF -> BF) -> [Value] -> Value
pmath f [BNum (BInt x)] = BNum $ BFloat $ f $ fromIntegral x
pmath f [BNum (BFloat x)] = BNum $ BFloat $ f x
pmath f [x] = BErr $ "Expected argument to be a number, got " ++ show x
pmath f x = BErr $ "Expected exactly one argument, got " ++ (show $ length x)


psqrt :: [Value] -> Value
psqrt = pmath sqrt

pcos :: [Value] -> Value
pcos = pmath cos

psine :: [Value] -> Value
psine = pmath sin

parc :: [Value] -> Value
parc = pmath atan

plog :: [Value] -> Value
plog = pmath log

e :: BF
e = exp 1

ppow :: [Value] -> Value
ppow = pmath (e **)

pbessel :: [Value] -> Value
pbessel [BNum (BInt x), BNum (BInt y)] = BNum $ BFloat $ jn x $ fromIntegral y
pbessel [BNum (BInt x), BNum (BFloat y)] = BNum $ BFloat $ jn x y
pbessel [x, BNum _] = BErr $ "Expected argument to be an integer, got " ++ show x
pbessel [_, x] = BErr $ "Expected argument to be a number, got " ++ show x
pbessel x = BErr $ "Expected exactly two arguments, got " ++ (show $ length x)
