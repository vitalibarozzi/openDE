module Physics.ODE.Raw.Objects (
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
)
where

import Data.Maybe
import Foreign
import Physics.ODE.Raw.Types
import Physics.ODE.Raw.Utilities

-- foreign import ccall unsafe "&dGeomDestroy" cDestroy :: FinalizerPtr GeomStruct
-- -------------------------------------------
--  Sphere class
createSphere :: Maybe Space -> ODEreal -> IO Geom
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
foreign import ccall unsafe "dCreateSphere"
    createSpheredCreateSphere ::
        Space ->
        ODEreal ->
        IO Geom
sphereSetRadius :: Geom -> ODEreal -> IO ()
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
foreign import ccall unsafe "dGeomSphereSetRadius"
    sphereSetRadiusdGeomSphereSetRadius ::
        Geom ->
        ODEreal ->
        IO ()
sphereGetRadius :: Geom -> IO ODEreal
sphereGetRadius = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- sphereGetRadiusdGeomSphereGetRadius marshaledArg_2
            return (ret_3)
        )
foreign import ccall unsafe "dGeomSphereGetRadius"
    sphereGetRadiusdGeomSphereGetRadius ::
        Geom ->
        IO ODEreal
spherePointDepth ::
    Geom ->
    ODEreal ->
    ODEreal ->
    ODEreal ->
    IO ODEreal
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
foreign import ccall unsafe "dGeomSpherePointDepth"
    spherePointDepthdGeomSpherePointDepth ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ODEreal

-- -------------------------------------------
--  Box class
createBox ::
    Maybe Space ->
    ODEreal ->
    ODEreal ->
    ODEreal ->
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
foreign import ccall unsafe "dCreateBox"
    createBoxdCreateBox ::
        Space ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO Geom
boxSetLengths :: Geom -> ODEreal -> ODEreal -> ODEreal -> IO ()
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
foreign import ccall unsafe "dGeomBoxSetLengths"
    boxSetLengthsdGeomBoxSetLengths ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ()
boxGetLengths :: Geom -> IO ((ODEreal, ODEreal, ODEreal))
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
foreign import ccall unsafe "dGeomBoxGetLengths"
    boxGetLengthsdGeomBoxGetLengths ::
        Geom ->
        Ptr ODEreal ->
        IO ()
boxPointDepth ::
    Geom ->
    ODEreal ->
    ODEreal ->
    ODEreal ->
    IO ODEreal
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
foreign import ccall unsafe "dGeomBoxPointDepth"
    boxPointDepthdGeomBoxPointDepth ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ODEreal

-- -------------------------------------------
--  Plane class
createPlane ::
    Maybe Space ->
    ODEreal ->
    ODEreal ->
    ODEreal ->
    ODEreal ->
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
foreign import ccall unsafe "dCreatePlane"
    createPlanedCreatePlane ::
        Space ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO Geom
planeSetParams ::
    Geom ->
    ODEreal ->
    ODEreal ->
    ODEreal ->
    ODEreal ->
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
foreign import ccall unsafe "dGeomPlaneSetParams"
    planeSetParamsdGeomPlaneSetParams ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ()
planeGetParams :: Geom -> IO ((ODEreal, ODEreal, ODEreal, ODEreal))
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
foreign import ccall unsafe "dGeomPlaneGetParams"
    planeGetParamsdGeomPlaneGetParams ::
        Geom ->
        Ptr ODEreal ->
        IO ()
planePointDepth ::
    Geom ->
    ODEreal ->
    ODEreal ->
    ODEreal ->
    IO ODEreal
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
foreign import ccall unsafe "dGeomPlanePointDepth"
    planePointDepthdGeomPlanePointDepth ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ODEreal

--  FIXME: Do the rest.
