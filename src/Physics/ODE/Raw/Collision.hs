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

The data argument is passed from dSpaceCollide directly to the callback function. Its meaning is user defined. The o1 and o2 arguments are the geoms that may be near each other.

The callback function can call dCollide on o1 and o2 to generate contact points between each pair. Then these contact points may be added to the simulation as contact joints. The user's callback function can of course chose not to call dCollide for any pair, e.g. if the user decides that those pairs should not interact.

Other spaces that are contained within the colliding space are not treated specially, i.e. they are not recursed into. The callback function may be passed these contained spaces as one or both geom arguments.

dSpaceCollide() is guaranteed to pass all intersecting geom pairs to the callback function, but it may also make mistakes and pass non-intersecting pairs. The number of mistaken calls depends on the internal algorithms used by the space. Thus you should not expect that dCollide will return contacts for every pair passed to the callback.

void dSpaceCollide2 (dGeomID o1, dGeomID o2, void *data, dNearCallback *callback);

-}
