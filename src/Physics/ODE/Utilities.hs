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

forceFinalization :: ForeignPtr a -> IO ()
{-# INLINE forceFinalization #-}
forceFinalization =
    finalizeForeignPtr

peekVector3 :: (Storable a) => Ptr a -> IO (a, a, a)
{-# INLINE peekVector3 #-}
peekVector3 ptr =
    do
        [x, y, z] <- peekArray 3 ptr
        return (x, y, z)

pokeVector3 :: (Storable a) => Ptr a -> (a, a, a) -> IO ()
{-# INLINE pokeVector3 #-}
pokeVector3 ptr (x, y, z) =
    pokeArray ptr [x, y, z]

peekVector4 :: (Storable a) => Ptr a -> IO (a, a, a, a)
{-# INLINE peekVector4 #-}
peekVector4 ptr =
    do
        [x, y, z, n] <- peekArray 4 ptr
        return (x, y, z, n)

peekMatrix :: (Storable a) => (Int, Int) -> Ptr a -> IO (Array (Int, Int) a)
{-# INLINE peekMatrix #-}
peekMatrix (x, y) ptr =
    do
        elts <- peekArray (x * y) ptr
        return (array ((0, 0), (x, y)) [((a, b), e) | e <- elts | a <- [0 .. x - 1], b <- [0 .. y - 1]])
