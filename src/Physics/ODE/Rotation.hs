module Physics.ODE.Rotation
    ( createMatrix3
--    , rToQ
    , peekMatrix3
    ) where

import Foreign

import Physics.ODE.Raw.Types
import Physics.ODE.Raw.Hsc
import Physics.ODE.Raw.Rotation


createMatrix3 :: IO Matrix3
createMatrix3 = do matrix <- mallocBytes sizeOfMatrix3
                   setIdentity matrix
                   return matrix


peekMatrix3 :: Matrix3 -> IO [Float]
peekMatrix3 = peekArray (3*4)
