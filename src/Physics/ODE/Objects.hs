module Physics.ODE.Objects (
    sphereRadius,
    --sphereSetRadius,
    --sphereGetRadius,
    planeParams,
    --planeSetParams,
    --planeGetParams,
    boxLengths,
    --boxSetLengths,
    --boxGetLengths,
    createSphere,
    createBox,
    createPlane,
    --
    spherePointDepth,
    boxPointDepth,
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

-----------------------------------------------------------
sphereRadius :: Geom -> StateVar Float
sphereRadius = undefined
sphereGetRadius :: Geom -> IO Float
sphereSetRadius :: Geom -> Float -> IO ()
sphereGetRadius = c'dGeomSphereGetRadius
sphereSetRadius = c'dGeomSphereSetRadius


-- -------------------------------------------
boxLengths :: Geom -> StateVar (Float, Float, Float)
boxLengths = undefined
boxSetLengths :: Geom -> Float -> Float -> Float -> IO ()
boxSetLengths = c'dGeomBoxSetLengths
boxGetLengths :: Geom -> IO ((Float, Float, Float))
boxGetLengths geom = 
    allocaArray
        4
        ( \arg -> do
            c'dGeomBoxGetLengths geom arg
            peekVector3 arg
        )

-----------------------------------------------------------
planeParams :: Geom -> StateVar (Float, Float, Float, Float)
planeParams = undefined
planeSetParams ::
    Geom ->
    Float ->
    Float ->
    Float ->
    Float ->
    IO ()
planeSetParams = c'dGeomPlaneSetParams
planeGetParams :: Geom -> IO ((Float, Float, Float, Float))
planeGetParams geom =
    allocaArray
        4
        ( \arr -> do
            c'dGeomPlaneGetParams geom arr
            peekVector4 arr
        )

-----------------------------------------------------------
planePointDepth ::
    Geom ->
    Float ->
    Float ->
    Float ->
    IO Float
planePointDepth = c'dGeomPlanePointDepth

-----------------------------------------------------------
boxPointDepth ::
    Geom ->
    Float ->
    Float ->
    Float ->
    IO Float
boxPointDepth = c'dGeomBoxPointDepth

------------------------------------------------
------------------------------------------------
------------------------------------------------

createSphere :: Maybe Space -> Float -> IO Geom
createSphere = \arg_0 arg_1 ->
    ( case arg_0 of
        Data.Maybe.Just a_2 -> \action_3 -> action_3 a_2
        Data.Maybe.Nothing -> \action_4 -> action_4 nullPtr
    )
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 -> do
                    c'dCreateSphere marshaledArg_5 marshaledArg_7
                )
        )

spherePointDepth ::
    Geom ->
    Float ->
    Float ->
    Float ->
    IO Float
spherePointDepth = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    c'dGeomSpherePointDepth marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                )
                        )
                )
        )

-- -------------------------------------------
createBox ::
    Maybe Space ->
    Float ->
    Float ->
    Float ->
    IO Geom
createBox = \arg_0 arg_1 arg_2 arg_3 ->
    ( case arg_0 of
        Data.Maybe.Just a_4 -> \action_5 -> action_5 a_4
        Data.Maybe.Nothing -> \action_6 -> action_6 nullPtr
    )
        ( \marshaledArg_7 ->
            (\action_8 -> action_8 arg_1)
                ( \marshaledArg_9 ->
                    (\action_10 -> action_10 arg_2)
                        ( \marshaledArg_11 ->
                            (\action_12 -> action_12 arg_3)
                                ( \marshaledArg_13 -> do
                                    c'dCreateBox marshaledArg_7 marshaledArg_9 marshaledArg_11 marshaledArg_13
                                )
                        )
                )
        )

-- -------------------------------------------
createPlane ::
    Maybe Space ->
    Float ->
    Float ->
    Float ->
    Float ->
    IO Geom
createPlane = \arg_0 arg_1 arg_2 arg_3 arg_4 ->
    ( case arg_0 of
        Data.Maybe.Just a_5 -> \action_6 -> action_6 a_5
        Data.Maybe.Nothing -> \action_7 -> action_7 nullPtr
    )
        ( \marshaledArg_8 ->
            (\action_9 -> action_9 arg_1)
                ( \marshaledArg_10 ->
                    (\action_11 -> action_11 arg_2)
                        ( \marshaledArg_12 ->
                            (\action_13 -> action_13 arg_3)
                                ( \marshaledArg_14 ->
                                    (\action_15 -> action_15 arg_4)
                                        ( \marshaledArg_16 -> do
                                            c'dCreatePlane marshaledArg_8 marshaledArg_10 marshaledArg_12 marshaledArg_14 marshaledArg_16
                                        )
                                )
                        )
                )
        )

--  FIXME: Do the rest.
--  ... what rest?
