{-# LANGUAGE LambdaCase #-}
module Physics.ODE.Objects (
    -- createCylinder,
    -- createRay,
    createSphere,
    createPlane,
    createBox,
    --ray,
    --rayLength,
    --cylinderParams
    sphereRadius,
    boxLengths,
    planeParams,
    --cylinderPointDepth,
    boxPointDepth,
    spherePointDepth,
    planePointDepth,
    -- * Raw
    module Physics.ODE.Raw.Objects
) where

import Data.Maybe
import Foreign
import Physics.ODE.Raw.Types
import Physics.ODE.Utilities
import Physics.ODE.Raw.Objects
import Data.StateVar
import Control.Monad.IO.Class

-----------------------------------------------------------
sphereRadius :: Geom -> StateVar Float
sphereRadius geom = do
    StateVar _get _set
  where
    _get = c'dGeomSphereGetRadius geom
    _set = c'dGeomSphereSetRadius geom


-- -------------------------------------------
boxLengths :: Geom -> StateVar (Float, Float, Float)
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

-----------------------------------------------------------
planeParams :: Geom -> StateVar (Float, Float, Float, Float)
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

-----------------------------------------------------------
planePointDepth ::
    (MonadIO m) => 
    Geom ->
    Float ->
    Float ->
    Float ->
    m Float
planePointDepth a b c d = 
    liftIO (c'dGeomPlanePointDepth a b c d)

-----------------------------------------------------------
boxPointDepth ::
    (MonadIO m) => 
    Geom ->
    Float ->
    Float ->
    Float ->
    m Float
boxPointDepth a b c d = 
    liftIO (c'dGeomBoxPointDepth a b c d)

------------------------------------------------
createSphere :: (MonadIO m) => Maybe Space -> Float -> m Geom
createSphere = \case
    Nothing -> \radius -> liftIO (c'dCreateSphere nullPtr radius)
    Just ptr -> \radius -> liftIO (c'dCreateSphere ptr radius)

-- -------------------------------------------
createPlane ::
    Maybe Space ->
    Float ->
    Float ->
    Float ->
    Float ->
    IO Geom
createPlane = \case
    Just a_5 -> \arg_1 arg_2 arg_3 arg_4 -> c'dCreatePlane  a_5 arg_1 arg_2 arg_3 arg_4
    Nothing -> \arg_1 arg_2 arg_3 arg_4 -> c'dCreatePlane  nullPtr arg_1 arg_2 arg_3 arg_4

-- -------------------------------------------
createBox ::
    Maybe Space ->
    Float ->
    Float ->
    Float ->
    IO Geom
createBox = \case 
    Just a_4 -> \arg_1 arg_2 arg_3 ->c'dCreateBox  a_4 arg_1 arg_2 arg_3
    Nothing -> \arg_1 arg_2 arg_3 ->c'dCreateBox nullPtr arg_1 arg_2 arg_3

------------------------------------------------
spherePointDepth ::
    (MonadIO m) =>
    Geom ->
    Float ->
    Float ->
    Float ->
    m Float
spherePointDepth a b c d = 
    liftIO (c'dGeomSpherePointDepth a b c d)
