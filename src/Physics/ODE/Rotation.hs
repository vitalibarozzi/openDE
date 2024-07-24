module Physics.ODE.Rotation (
    createMatrix3,
    peekMatrix3,
    -- , rToQ
) where

import Control.Monad.IO.Class
import Foreign
import Physics.ODE.Raw.Hsc
import Physics.ODE.Raw.Rotation
import Physics.ODE.Raw.Types

-----------------------------------------------------------
createMatrix3 :: (MonadIO m) => m Matrix3
{-# INLINE createMatrix3 #-}
createMatrix3 =
    liftIO $ do
        matrix <- mallocBytes sizeOfMatrix3
        setIdentity matrix
        return matrix

-----------------------------------------------------------
peekMatrix3 :: (MonadIO m) => Matrix3 -> m [Float]
{-# INLINE peekMatrix3 #-}
peekMatrix3 = do
    liftIO . peekArray (3 * 4)
