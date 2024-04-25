module Physics.ODE.Collision
    ( collide
    , spaceCollide
    ) where

import Foreign

import Physics.ODE.Types
import Physics.ODE.Hsc
-- import Physics.ODE.Utilities

foreign import ccall unsafe "memset" cMemset :: Ptr a -> Int -> Int -> IO (Ptr a)

foreign import ccall unsafe "dCollide" rawCollide
    :: Ptr GeomStruct -> Ptr GeomStruct -> Int -> Ptr ContactGeom -> Int -> IO Int

collide :: Geom -> Geom -> Int -> IO [ContactInfo]
collide geom1 geom2 nElems
    = allocaArray nElems $ \points ->
      do cMemset points 0 (sizeOfContactInfo * nElems)
         ret <- rawCollide geom1 geom2 nElems (addressOfGeom points) sizeOfContactInfo
         peekArray ret points
    where sizeOfContactInfo = sizeOf (undefined :: ContactInfo)

type RawCallback = Ptr () -> Ptr GeomStruct -> Ptr GeomStruct -> IO ()
type Callback = Geom -> Geom -> IO ()

foreign import ccall unsafe "wrapper" mkRawCallback :: RawCallback -> IO (FunPtr RawCallback)
mkCallback :: Callback -> IO (FunPtr RawCallback)
mkCallback cb
    = mkRawCallback rawCb
    where rawCb _data geom1 geom2
              = cb geom1 geom2

foreign import ccall safe "dSpaceCollide" cSpaceCollide
    :: Ptr SpaceStruct -> Ptr () -> FunPtr RawCallback -> IO ()

spaceCollide :: Space -> Callback -> IO ()
spaceCollide space callback
    = mkCallback callback >>= \rawCallback ->
      cSpaceCollide space nullPtr rawCallback >>
      freeHaskellFunPtr rawCallback

