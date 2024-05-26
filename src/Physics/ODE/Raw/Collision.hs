module Physics.ODE.Raw.Collision where

import Foreign
import Physics.ODE.Raw.Types

type RawCallback = Ptr () -> Ptr GeomStruct -> Ptr GeomStruct -> IO ()

foreign import ccall unsafe "memset" cMemset :: Ptr a -> Int -> Int -> IO (Ptr a)
foreign import ccall unsafe "dCollide" rawCollide :: Ptr GeomStruct -> Ptr GeomStruct -> Int -> Ptr ContactGeom -> Int -> IO Int
foreign import ccall unsafe "wrapper" mkRawCallback :: RawCallback -> IO (FunPtr RawCallback)
foreign import ccall safe "dSpaceCollide" cSpaceCollide :: Ptr SpaceStruct -> Ptr () -> FunPtr RawCallback -> IO ()
