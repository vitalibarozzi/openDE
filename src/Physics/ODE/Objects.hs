{-# LANGUAGE LambdaCase #-}
module Physics.ODE.Objects (
    createCylinder,
    createRay,
    createSphere,
    createPlane,
    createBox,
    ray,
    rayLength,
    cylinderParams,
    sphereRadius,
    boxLengths,
    planeParams,
    cylinderPointDepth,
    boxPointDepth,
    spherePointDepth,
    planePointDepth,
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
    _get =
        allocaArray
            4
            ( \arg -> do
                c'dGeomRayGet geom arg
                peekVector6 arg
        )

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
planeParams :: Geom -> StateVar (Float, Float, Float, Float)
{-# INLINE planeParams #-}
planeParams geom = do
    StateVar _get _set
  where
    _set (a,b,c,d) = c'dGeomPlaneSetParams geom a b c d
    _get = 
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
    _set (a,b) = c'dGeomCCylinderSetParams geom a b
    _get = 
        allocaArray
            4
            ( \arr -> do
                c'dGeomCCylinderGetParams geom arr
                peekVector2 arr
            )

------------------------------------------------------------
planePointDepth ::
    (MonadIO m) => 
    Geom ->
    Float ->
    Float ->
    Float ->
    m Float
{-# INLINE planePointDepth #-}
planePointDepth a b c d = 
    liftIO (c'dGeomPlanePointDepth a b c d)


------------------------------------------------------------
cylinderPointDepth ::
    (MonadIO m) => 
    Geom ->
    Float ->
    Float ->
    Float ->
    m Float
{-# INLINE cylinderPointDepth #-}
cylinderPointDepth a b c d = 
    liftIO (c'dGeomCCylinderPointDepth a b c d)

------------------------------------------------------------
boxPointDepth ::
    (MonadIO m) => 
    Geom ->
    Float ->
    Float ->
    Float ->
    m Float
{-# INLINE boxPointDepth #-}
boxPointDepth a b c d = 
    liftIO (c'dGeomBoxPointDepth a b c d)

------------------------------------------------------------
createSphere :: (MonadIO m) => Maybe Space -> Float -> m Geom
{-# INLINE createSphere #-}
createSphere = \case
    Nothing  -> liftIO . c'dCreateSphere nullPtr
    Just ptr -> liftIO . c'dCreateSphere ptr

------------------------------------------------------------
createCylinder :: (MonadIO m) => Maybe Space -> Float -> Float -> m Geom
{-# INLINE createCylinder #-}
createCylinder = \case
    Nothing  -> \a b -> liftIO (c'dCreateCCylinder nullPtr a b)
    Just ptr -> \a b -> liftIO (c'dCreateCCylinder ptr a b)

------------------------------------------------------------
createRay :: (MonadIO m) => Maybe Space -> Float -> m Geom
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
    Geom ->
    Float ->
    Float ->
    Float ->
    m Float
{-# INLINE spherePointDepth #-}
spherePointDepth a b c d = 
    liftIO (c'dGeomSpherePointDepth a b c d)
