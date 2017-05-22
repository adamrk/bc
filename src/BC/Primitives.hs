module BC.Primitives where

import BC.Types

primitives :: [(String, Value)]
primitives = map valuize primitivesList
  where valuize (x, y) = (x, BNative y)


primitivesList :: [(String, [Value] -> Value)]
primitivesList = [ ("cos", pcos)
                 , ("sqrt", psqrt)
                 ]


psqrt :: [Value] -> Value
psqrt [BNum (BInt x)] = BNum $ BFloat $ sqrt $ fromIntegral x
psqrt [BNum (BFloat x)] = BNum $ BFloat $ sqrt x
psqrt [x] = BErr $ "Expected argument to be a number, got " ++ show x


pcos :: [Value] -> Value
pcos [BNum (BInt x)] = BNum $ BFloat $ cos $ fromIntegral x
pcos [BNum (BFloat x)] = BNum $ BFloat $ cos x
pcos [x] = BErr $ "Expected argument to be a number, got " ++ show x

