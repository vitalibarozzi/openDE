{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Physics.ODE.Objects (
    createSphere,
    sphereSetRadius,
    sphereGetRadius,
    spherePointDepth,
    createBox,
    boxSetLengths,
    boxGetLengths,
    boxPointDepth,
    createPlane,
    planeSetParams,
    planeGetParams,
    planePointDepth,
) where

import Data.Maybe
import Foreign
import Physics.ODE.Raw.Types
import Physics.ODE.Utilities
import Physics.ODE.Raw.Objects

-- foreign import ccall unsafe "&dGeomDestroy" cDestroy :: FinalizerPtr GeomStruct
-- -------------------------------------------
--  Sphere class
createSphere :: Maybe Space -> Float -> IO Geom
createSphere = \arg_0 arg_1 ->
    ( case arg_0 of
        Data.Maybe.Just a_2 -> \action_3 -> action_3 a_2
        Data.Maybe.Nothing -> \action_4 -> action_4 nullPtr
    )
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 -> do
                    ret_8 <- createSpheredCreateSphere marshaledArg_5 marshaledArg_7
                    return (ret_8)
                )
        )
sphereSetRadius :: Geom -> Float -> IO ()
sphereSetRadius = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- sphereSetRadiusdGeomSphereSetRadius marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )
sphereGetRadius :: Geom -> IO Float
sphereGetRadius = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- sphereGetRadiusdGeomSphereGetRadius marshaledArg_2
            return (ret_3)
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
                                    ret_12 <- spherePointDepthdGeomSpherePointDepth marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    return (ret_12)
                                )
                        )
                )
        )

-- -------------------------------------------
--  Box class
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
                                    ret_14 <- createBoxdCreateBox marshaledArg_7 marshaledArg_9 marshaledArg_11 marshaledArg_13
                                    return (ret_14)
                                )
                        )
                )
        )
boxSetLengths :: Geom -> Float -> Float -> Float -> IO ()
boxSetLengths = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- boxSetLengthsdGeomBoxSetLengths marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> do return ()
                                )
                        )
                )
        )
boxGetLengths :: Geom -> IO ((Float, Float, Float))
boxGetLengths = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 ->
            allocaArray
                4
                ( \marshaledArg_3 -> do
                    ret_4 <- boxGetLengthsdGeomBoxGetLengths marshaledArg_2 marshaledArg_3
                    peekVector3 (marshaledArg_3)
                )
        )
boxPointDepth ::
    Geom ->
    Float ->
    Float ->
    Float ->
    IO Float
boxPointDepth = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- boxPointDepthdGeomBoxPointDepth marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    return (ret_12)
                                )
                        )
                )
        )

-- -------------------------------------------
--  Plane class
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
                                            ret_17 <- createPlanedCreatePlane marshaledArg_8 marshaledArg_10 marshaledArg_12 marshaledArg_14 marshaledArg_16
                                            return (ret_17)
                                        )
                                )
                        )
                )
        )
planeSetParams ::
    Geom ->
    Float ->
    Float ->
    Float ->
    Float ->
    IO ()
planeSetParams = \arg_0 arg_1 arg_2 arg_3 arg_4 ->
    (\action_5 -> action_5 arg_0)
        ( \marshaledArg_6 ->
            (\action_7 -> action_7 arg_1)
                ( \marshaledArg_8 ->
                    (\action_9 -> action_9 arg_2)
                        ( \marshaledArg_10 ->
                            (\action_11 -> action_11 arg_3)
                                ( \marshaledArg_12 ->
                                    (\action_13 -> action_13 arg_4)
                                        ( \marshaledArg_14 -> do
                                            ret_15 <- planeSetParamsdGeomPlaneSetParams marshaledArg_6 marshaledArg_8 marshaledArg_10 marshaledArg_12 marshaledArg_14
                                            case () of
                                                () -> do return ()
                                        )
                                )
                        )
                )
        )
planeGetParams :: Geom -> IO ((Float, Float, Float, Float))
planeGetParams = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 ->
            allocaArray
                4
                ( \marshaledArg_3 -> do
                    ret_4 <- planeGetParamsdGeomPlaneGetParams marshaledArg_2 marshaledArg_3
                    peekVector4 (marshaledArg_3)
                )
        )
planePointDepth ::
    Geom ->
    Float ->
    Float ->
    Float ->
    IO Float
planePointDepth = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- planePointDepthdGeomPlanePointDepth marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    return (ret_12)
                                )
                        )
                )
        )

--  FIXME: Do the rest.
