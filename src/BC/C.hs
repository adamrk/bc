{-# LANGUAGE ForeignFunctionInterface #-}
module BC.C where

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "math.h jn"
     c_jn :: CInt -> CDouble -> CDouble

jn :: (Real a, Fractional a) => Integer -> a -> a
jn x y = realToFrac $ c_jn (fromIntegral x) (realToFrac y)
