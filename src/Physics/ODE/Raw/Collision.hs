module Physics.ODE.Raw.Collision where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "memset" cMemset            :: Ptr a -> Int -> Int -> IO (Ptr a)
foreign import ccall unsafe "wrapper" mkRawCallback     :: RawCallback -> IO (FunPtr RawCallback)
---------------
foreign import ccall unsafe "dCollide" rawCollide       :: Ptr GeomStruct -> Ptr GeomStruct -> Int -> Ptr ContactGeom -> Int -> IO Int
foreign import ccall safe "dSpaceCollide" cSpaceCollide :: Ptr SpaceStruct -> Ptr () -> FunPtr RawCallback -> IO ()

-- TODO test it
foreign import ccall safe "dInfinity" cInfinity :: Float

{-
void dSetColliderOverride (int class1, int class2, dColliderFn *fn);
dColliderFn is defined as:
typedef int dColliderFn (dGeomID o1, dGeomID o2, int flags, dContactGeom *contact, int skip);
typedef void dNearCallback (void *data, dGeomID o1, dGeomID o2);
void dSpaceCollide2 (dGeomID o1, dGeomID o2, void *data, dNearCallback *callback);
-}
