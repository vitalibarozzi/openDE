{-# LANGUAGE ParallelListComp #-}

module Physics.ODE.Utilities (
    forceFinalization,
    peekVector3,
    pokeVector3,
    peekVector4,
    peekMatrix,
)
where

import Foreign
import Data.Array
import Control.Monad.IO.Class


-----------------------------------------------------------
forceFinalization :: (MonadIO m) => ForeignPtr a -> m ()
{-# INLINE forceFinalization #-}
forceFinalization =
    liftIO . finalizeForeignPtr

-----------------------------------------------------------
peekVector3 :: (MonadFail m, MonadIO m, Storable a) => Ptr a -> m (a, a, a)
{-# INLINE peekVector3 #-}
peekVector3 ptr = do
    [x, y, z] <- liftIO (peekArray 3 ptr)
    return (x, y, z)

-----------------------------------------------------------
pokeVector3 :: (MonadIO m, Storable a) => Ptr a -> (a, a, a) -> m ()
{-# INLINE pokeVector3 #-}
pokeVector3 ptr (x, y, z) =
    liftIO (pokeArray ptr [x, y, z])

-----------------------------------------------------------
peekVector4 :: (MonadFail m, MonadIO m, Storable a) => Ptr a -> m (a, a, a, a)
{-# INLINE peekVector4 #-}
peekVector4 ptr = do
    [x, y, z, n] <- liftIO (peekArray 4 ptr)
    return (x, y, z, n)

-----------------------------------------------------------
peekMatrix :: (MonadIO m, Storable a) => (Int, Int) -> Ptr a -> m (Array (Int, Int) a)
{-# INLINE peekMatrix #-}
peekMatrix (x, y) ptr = do
    elts <- liftIO (peekArray (x * y) ptr)
    return (array ((0, 0), (x, y)) [((a, b), e) | e <- elts | a <- [0 .. x - 1], b <- [0 .. y - 1]])
