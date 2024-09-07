{-# LANGUAGE LambdaCase #-}
-- | 100% complete.
module Physics.ODE.Objects (
    boxLengths,
    boxPointDepth,
    capsuleParams,
    capsulePointDepth,
    createBox,
    createCapsule,
    createCylinder,
    createPlane,
    createRay,
    createSphere,
    cylinderParams,
    planeParams,
    planePointDepth,
    ray,
    rayClosestHit,
    rayLength,
    rayParams,
    spherePointDepth,
    sphereRadius,
    module Physics.ODE.Raw.Objects
) where

import Foreign
import Physics.ODE.Raw.Types
import Physics.ODE.Utilities
import Physics.ODE.Raw.Objects
import Data.StateVar
import Control.Monad.IO.Class


------------------------------------------------------------
-- | Get/Set the length of the given ray.
ray :: Geom -> StateVar (Float,Float,Float,Float,Float,Float)
{-# INLINE ray #-}
ray geom = do
    StateVar _get _set
  where
    _set (a,b,c,d,e,f) = c'dGeomRaySet geom a b c d e f
    _get = do 
        pa <- mallocArray (4 * 3)
        pb <- mallocArray (4 * 3)
        () <- c'dGeomRayGet geom pa pb
        [x,y,z] <- peekArray 3 pa
        [w,k,l] <- peekArray 3 pb
        free pa
        free pb
        return (x,y,z,w,k,l)

------------------------------------------------------------
-- | Set or Get parameters for the ray which determine which 
-- hit between the ray geom and a trimesh geom is returned 
-- from dCollide. FirstContact determines if dCollide returns 
-- the first collision detected between the ray geom and a trimesh 
-- geom, even if that collision is not the nearest to the ray start 
-- position. BackfaceCull determines if dCollide returns a collision 
-- between the ray geom and a trimesh geom when the collision is 
-- between the ray and a backfacing triangle. Default values are 
-- FirstContact = 0, BackfaceCull = 0 (both false). 
rayParams :: Geom -> StateVar (Int,Int)
{-# INLINE rayParams #-}
rayParams geom = do
    StateVar _get _set
  where
    _set (a,b) = c'dGeomRaySetParams geom a b
    _get = do
        pa <- mallocArray 4
        pb <- mallocArray 4
        () <- c'dGeomRayGetParams geom pa pb
        x <- peek pa
        y <- peek pb
        free pa
        free pb
        return (x,y)

------------------------------------------------------------
-- | Set/Get the parameters of the given plane.
planeParams :: Geom -> StateVar (Float, Float, Float, Float)
{-# INLINE planeParams #-}
planeParams geom = do
    StateVar _get _set
  where
    _set (a,b,c,d) = c'dGeomPlaneSetParams geom a b c d
    _get = do
        allocaArray
            4
            ( \arr -> do
                c'dGeomPlaneGetParams geom arr
                peekVector4 arr
            )

------------------------------------------------------------
-- | Set/Get the parameters of the given cylinder.
cylinderParams :: Geom -> StateVar (Float, Float)
{-# INLINE cylinderParams #-}
cylinderParams geom = do
    StateVar _get _set
  where
    _set (a,b) = c'dGeomCylinderSetParams geom a b
    _get = do
        pa <- mallocArray 4
        pb <- mallocArray 4
        () <- c'dGeomCylinderGetParams geom pa pb
        x <- peek pa
        y <- peek pb
        free pa
        free pb
        return (x,y)

------------------------------------------------------------
-- | Set/Get the parameters of the given capsule.
capsuleParams :: Geom -> StateVar (Float, Float)
{-# INLINE capsuleParams #-}
capsuleParams geom = do
    StateVar _get _set
  where
    _set (a,b) = c'dGeomCapsuleSetParams geom a b
    _get = do
        pa <- mallocArray 4
        pb <- mallocArray 4
        () <- c'dGeomCapsuleGetParams geom pa pb
        x <- peek pa
        y <- peek pb
        free pa
        free pb
        return (x,y)

------------------------------------------------------------
-- | ClosestHit determines if dCollide returns the closest 
-- hit between the ray and a trimesh geom. If ClosestHit is 
-- false, the hit returned by dCollide may not be the nearest 
-- collision to the ray position. This parameter is ignored 
-- if FirstContact is set to true in dGeomRaySetParams(). If 
-- ClosestHit is set to true and BackfaceCull is set to false, 
-- the hit returned by dCollide may be between the ray and a 
-- backfacing triangle. The default value 
-- is ClosestHit = 0 (false). 
rayClosestHit :: Geom -> StateVar Int
{-# INLINE rayClosestHit #-}
rayClosestHit geom = do
    StateVar _get _set
  where
    _set a = c'dGeomRaySetClosestHit geom a
    _get = c'dGeomRayGetClosestHit geom

------------------------------------------------------------
-- | Set/Get the radius of the given sphere. 
sphereRadius :: Geom -> StateVar Float
{-# INLINE sphereRadius #-}
sphereRadius geom = do
    StateVar _get _set
  where
    _get = c'dGeomSphereGetRadius geom
    _set = c'dGeomSphereSetRadius geom

------------------------------------------------------------
-- | Set/Get the side lengths of the given box.
boxLengths :: Geom -> StateVar (Float, Float, Float)
{-# INLINE boxLengths #-}
boxLengths geom = do
    StateVar _get _set
  where
    _set (a,b,c) = c'dGeomBoxSetLengths geom a b c 
    _get =
        allocaArray
            4
            ( \arg -> do
                c'dGeomBoxGetLengths geom arg
                peekVector3 arg
        )

------------------------------------------------------------
-- | Set/Get the length of the given ray.
rayLength :: Geom -> StateVar Float
{-# INLINE rayLength #-}
rayLength geom = do
    StateVar _get _set
  where
    _set = c'dGeomRaySetLength geom
    _get = c'dGeomRayGetLength geom

------------------------------------------------------------
-- | Return the depth of the point (x,y,z) in the given plane. 
-- Points inside the geom will have positive depth, points 
-- outside it will have negative depth, and points on the 
-- surface will have zero depth.
-- Note that planes in ODE are in fact not really planes: they 
-- are half-spaces. Anything that is moving inside the half-space 
-- will be ejected out from it. This means that planes are only 
-- planes from the perspective of one side. If you want your 
-- planes to be reversed, multiply the whole plane equation by -1. 
planePointDepth ::
    (MonadIO m) => 
    Float ->
    Float ->
    Float ->
    Geom ->
    m Float
{-# INLINE planePointDepth #-}
planePointDepth b c d a = 
    liftIO (c'dGeomPlanePointDepth a b c d)


------------------------------------------------------------
-- | Return the depth of the point (x,y,z) in the given 
-- capsule. Points inside the geom will have positive depth, 
-- points outside it will have negative depth, and points on 
-- the surface will have zero depth.
capsulePointDepth ::
    (MonadIO m) => 
    Float ->
    Float ->
    Float ->
    Geom ->
    m Float
{-# INLINE capsulePointDepth #-}
capsulePointDepth b c d a = 
    liftIO (c'dGeomCapsulePointDepth a b c d)

------------------------------------------------------------
-- | Return the depth of the point (x,y,z) in the given box. 
-- Points inside the geom will have positive depth, points 
-- outside it will have negative depth, and points on the 
-- surface will have zero depth. 
boxPointDepth ::
    (MonadIO m) => 
    Float ->
    Float ->
    Float ->
    Geom ->
    m Float
{-# INLINE boxPointDepth #-}
boxPointDepth b c d a = 
    liftIO (c'dGeomBoxPointDepth a b c d)

------------------------------------------------------------
-- | Create a sphere geom of the given radius, and return its 
-- ID. If space is nonzero, insert it into that space. The 
-- point of reference for a sphere is its center. 
createSphere :: 
    (MonadIO m) => 
    Maybe Space -> 
    Float -> 
    m Geom
{-# INLINE createSphere #-}
createSphere = \case
    Nothing  -> liftIO . c'dCreateSphere nullPtr
    Just ptr -> liftIO . c'dCreateSphere ptr

------------------------------------------------------------
-- | Create a cylinder geom of the given parameters, and 
-- return its ID. If space is nonzero, insert it into that 
-- space. 
createCylinder :: 
     (MonadIO m) => 
     Maybe Space -> 
     Float -> 
     Float -> 
     m Geom
{-# INLINE createCylinder #-}
createCylinder = \case
    Nothing  -> \a b -> liftIO (c'dCreateCylinder nullPtr a b)
    Just ptr -> \a b -> liftIO (c'dCreateCylinder ptr a b)

------------------------------------------------------------
-- | Create a capsule geom of the given parameters, and 
-- return its ID. If space is nonzero, insert it into that 
-- space.
-- A capsule is like a normal cylinder except it has 
-- half-sphere caps at its ends. This feature makes the 
-- internal collision detection code particularly fast and 
-- accurate. The cylinder's length, not counting the caps, 
-- is given by length. The cylinder is aligned along the 
-- geom's local Z axis. The radius of the caps, and of the 
-- cylinder itself, is given by radius. 
createCapsule :: 
    (MonadIO m) => 
    Maybe Space -> 
    Float -> 
    Float -> 
    m Geom
{-# INLINE createCapsule #-}
createCapsule = \case
    Nothing  -> \a b -> liftIO (c'dCreateCapsule nullPtr a b)
    Just ptr -> \a b -> liftIO (c'dCreateCapsule ptr a b)

------------------------------------------------------------
-- | Create a ray geom of the given length, and return its 
-- ID. If space is nonzero, insert it into that space. 
createRay :: 
    (MonadIO m) => 
    Maybe Space -> 
    Float -> 
    m Geom
{-# INLINE createRay #-}
createRay = \case
    Nothing  -> liftIO . c'dCreateRay nullPtr
    Just ptr -> liftIO . c'dCreateRay ptr

------------------------------------------------------------
-- | Create a plane geom of the given parameters, and return 
-- its ID. If space is nonzero, insert it into that space. 
-- The plane equation is a*x+b*y+c*z = d The plane's normal 
-- vector is (a,b,c), and it must have length 1. Planes are 
-- non-placeable geoms. This means that, unlike placeable 
-- geoms, planes do not have an assigned position and rotation. 
-- This means that the parameters (a,b,c,d) are always in global 
-- coordinates. In other words it is assumed that the plane is 
-- always part of the static environment and not tied to any 
-- movable object. 
createPlane ::
    (MonadIO m) =>
    Maybe Space ->
    Float ->
    Float ->
    Float ->
    Float ->
    m Geom
{-# INLINE createPlane #-}
createPlane = \case
    Just a_5 -> \a b c d -> liftIO (c'dCreatePlane a_5 a b c d)
    Nothing  -> \a b c d -> liftIO (c'dCreatePlane nullPtr a b c d)

------------------------------------------------------------
-- | Create a box geom of the given x/y/z side lengths 
-- (lx,ly,lz), and return its ID. If space is nonzero, insert 
-- it into that space. The point of reference for a box is 
-- its center. 
createBox ::
    (MonadIO m) =>
    Maybe Space ->
    Float ->
    Float ->
    Float ->
    m Geom
{-# INLINE createBox #-}
createBox = \case 
    Just a_4 -> \a b c -> liftIO (c'dCreateBox a_4 a b c) 
    Nothing  -> \a b c -> liftIO (c'dCreateBox nullPtr a b c)

------------------------------------------------------------
-- | Return the depth of the point (x,y,z) in the given 
-- sphere. Points inside the geom will have positive depth, 
-- points outside it will have negative depth, and points on 
-- the surface will have zero depth. 
spherePointDepth ::
    (MonadIO m) =>
    Float ->
    Float ->
    Float ->
    Geom ->
    m Float
{-# INLINE spherePointDepth #-}
spherePointDepth b c d a = 
    liftIO (c'dGeomSpherePointDepth a b c d)
