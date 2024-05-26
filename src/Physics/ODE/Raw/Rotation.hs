module Physics.ODE.Raw.Rotation
    ( createMatrix3
    , setIdentity
    , fromAxisAndAngle
    , fromEulerAngles
--    , rToQ
    , peekMatrix3
    ) where

import Foreign

import Physics.ODE.Raw.Types
import Physics.ODE.Raw.Hsc

createMatrix3 :: IO Matrix3
createMatrix3 = do matrix <- mallocBytes sizeOfMatrix3
                   setIdentity matrix
                   return matrix

foreign import ccall unsafe "dRSetIdentity" setIdentity :: Matrix3 -> IO ()
foreign import ccall unsafe "dRFromAxisAndAngle" fromAxisAndAngle
    :: Matrix3 -> ODEreal -> ODEreal -> ODEreal -> ODEreal -> IO ()
foreign import ccall unsafe "dRFromEulerAngles" fromEulerAngles
    :: Matrix3 -> ODEreal -> ODEreal -> ODEreal -> IO ()

{-foreign import ccall unsafe "dRtoQ" dRtoQ
    :: Matrix3 -> Ptr ODEreal -> IO ()

rToQ :: Matrix3 -> IO Quaternion
rToQ matrix
    = allocaArray 4 $ \ptr ->
      do dRtoQ matrix ptr
         [a,b,c,d] <- peekArray 4 ptr
         return (a,b,c,d) -}


peekMatrix3 :: Matrix3 -> IO [ODEreal]
peekMatrix3 = peekArray (3*4)
