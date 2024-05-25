{-# LANGUAGE ParallelListComp #-}
module Physics.ODE.Utilities where

import Foreign

import Data.Array

forceFinalization :: ForeignPtr a -> IO ()
forceFinalization = finalizeForeignPtr

peekVector3 :: Storable a => Ptr a -> IO (a,a,a)
peekVector3 ptr
    = do [x,y,z] <- peekArray 3 ptr
         return (x,y,z)

pokeVector3 :: Storable a => Ptr a -> (a,a,a) -> IO ()
pokeVector3 ptr (x,y,z)
    = pokeArray ptr [x,y,z]

peekVector4 :: Storable a => Ptr a -> IO (a,a,a,a)
peekVector4 ptr
    = do [x,y,z,n] <- peekArray 4 ptr
         return (x,y,z,n)

peekMatrix :: Storable a => (Int,Int) -> Ptr a -> IO (Array (Int,Int) a)
peekMatrix (x,y) ptr
    = do elts <- peekArray (x*y) ptr
         return (array ((0,0),(x,y)) [((a,b),e) | e <- elts | a <- [0..x-1], b <- [0..y-1]])
