module Physics.ODE.Raw.Rotation
    ( setIdentity
    , fromAxisAndAngle
    , fromEulerAngles
    ) where

import Physics.ODE.Raw.Types



foreign import ccall unsafe "dRSetIdentity" setIdentity :: Matrix3 -> IO ()
foreign import ccall unsafe "dRFromAxisAndAngle" fromAxisAndAngle :: Matrix3 -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dRFromEulerAngles" fromEulerAngles :: Matrix3 -> Float -> Float -> Float -> IO ()



{-foreign import ccall unsafe "dRtoQ" dRtoQ
    :: Matrix3 -> Ptr Float -> IO ()

rToQ :: Matrix3 -> IO Quaternion
rToQ matrix
    = allocaArray 4 $ \ptr ->
      do dRtoQ matrix ptr
         [a,b,c,d] <- peekArray 4 ptr
         return (a,b,c,d) -}


