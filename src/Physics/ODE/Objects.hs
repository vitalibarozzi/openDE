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
rayClosestHit :: Geom -> StateVar Int
{-# INLINE rayClosestHit #-}
rayClosestHit geom = do
    StateVar _get _set
  where
    _set a = c'dGeomRaySetClosestHit geom a
    _get = c'dGeomRayGetClosestHit geom

------------------------------------------------------------
sphereRadius :: Geom -> StateVar Float
{-# INLINE sphereRadius #-}
sphereRadius geom = do
    StateVar _get _set
  where
    _get = c'dGeomSphereGetRadius geom
    _set = c'dGeomSphereSetRadius geom

------------------------------------------------------------
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
rayLength :: Geom -> StateVar Float
{-# INLINE rayLength #-}
rayLength geom = do
    StateVar _get _set
  where
    _set = c'dGeomRaySetLength geom
    _get = c'dGeomRayGetLength geom

------------------------------------------------------------
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
