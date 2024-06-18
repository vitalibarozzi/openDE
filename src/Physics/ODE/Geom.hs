module Physics.ODE.Geom (

    destroyGeom,

    setBody,
    getBody,
    --
    setGeomPosition,
    getGeomPosition,
    --
    setGeomQuaternion,
    getGeomQuaternion,
    --
    setGeomRotation,
    getGeomRotation,

    enableGeom,
    disableGeom,

    setGeomData,
    getGeomData,

    setRawGeomData,
    getRawGeomData,

    setSafeGeomData,
    getSafeGeomData,
    --
    --
    tryGetSafeGeomData,
    getBodyUnsafe,
    isSpace,
    getSpace,
    getClass,
    isGeomEnabled,
)
where

import Data.Maybe
import Data.Typeable
import Foreign
import Physics.ODE.Raw.Geom
import Physics.ODE.Raw.Hsc
import Physics.ODE.Raw.Types
import Physics.ODE.Raw.Utilities

-----------------------------------------------------------
destroyGeom :: Geom -> IO ()
destroyGeom arg_0 = (\action_1 -> action_1 arg_0)
        destroyGeomdGeomDestroy

-----------------------------------------------------------
getBody :: Geom -> IO (Maybe Body)
getBody arg_0 = (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getBodydGeomGetBody marshaledArg_2
            if ret_3 == nullPtr
                then return Nothing
                else return (Just ret_3)
        )
setBody :: Geom -> Maybe Body -> IO ()
setBody arg_0 arg_1 = (\action_2 -> action_2 arg_0)
        ( ( case arg_1 of
                Data.Maybe.Just a_4 -> \action_5 -> action_5 a_4
                Data.Maybe.Nothing -> \action_6 -> action_6 nullPtr
            ) . setBodydGeomSetBody
        )

-----------------------------------------------------------
enableGeom :: Geom -> IO ()
enableGeom arg_0 = (\action_1 -> action_1 arg_0)
        enableGeomdGeomEnable
disableGeom :: Geom -> IO ()
disableGeom arg_0 = (\action_1 -> action_1 arg_0)
        disableGeomdGeomDisable

-----------------------------------------------------------
getGeomRotation :: Geom -> IO Matrix3
getGeomRotation arg_0 = (\action_1 -> action_1 arg_0)
        getGeomRotationdGeomGetRotation
setGeomRotation :: Geom -> Matrix3 -> IO ()
setGeomRotation arg_0 arg_1 = (\action_2 -> action_2 arg_0)
        ( (\action_4 -> action_4 arg_1) . setGeomRotationdGeomSetRotation
        )

-----------------------------------------------------------
getGeomQuaternion ::
    Geom ->
    IO (ODEreal, ODEreal, ODEreal, ODEreal)
getGeomQuaternion arg_0 = (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 ->
            allocaArray
                4
                ( \marshaledArg_3 -> do
                    ret_4 <- getGeomQuaterniondGeomGetQuaternion marshaledArg_2 marshaledArg_3
                    peekVector4 marshaledArg_3
                )
        )
setGeomQuaternion ::
    Geom ->
    (ODEreal, ODEreal, ODEreal, ODEreal) ->
    IO ()
setGeomQuaternion arg_0 arg_1 = (\action_2 -> action_2 arg_0)
        ( ( \action_4 ->
                allocaArray
                    4
                    ( \ptr_5 ->
                        (>>)
                            ( pokeArray
                                ptr_5
                                ( case arg_1 of
                                    ( a_6
                                        , b_7
                                        , c_8
                                        , d_9
                                        ) ->
                                            [ a_6
                                            , b_7
                                            , c_8
                                            , d_9
                                            ]
                                )
                            )
                            (action_4 ptr_5)
                    )
            ) . setGeomQuaterniondGeomSetQuaternion
        )


-----------------------------------------------------------
getGeomPosition :: Geom -> IO (ODEreal, ODEreal, ODEreal)
getGeomPosition arg_0 = (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getGeomPositiondGeomGetPosition marshaledArg_2
            peekVector3 ret_3
        )
setGeomPosition :: Geom -> ODEreal -> ODEreal -> ODEreal -> IO ()
setGeomPosition arg_0 arg_1 arg_2 arg_3 = (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( (\action_10 -> action_10 arg_3) . setGeomPositiondGeomSetPosition marshaledArg_5 marshaledArg_7
                        )
                )
        )



-----------------------------------------------------------
isSpace :: Geom -> IO Bool
isSpace arg_0 = (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- isSpacedGeomIsSpace marshaledArg_2
            return (toBool ret_3)
        )

-----------------------------------------------------------
getSpace :: Geom -> IO (Maybe Space)
getSpace arg_0 = (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getSpacedGeomGetSpace marshaledArg_2
            if ret_3 == nullPtr
                then return Nothing
                else return (Just ret_3)
        )

-----------------------------------------------------------
getClass :: Geom -> IO GeomClass
getClass arg_0 = (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getClassdGeomGetClass marshaledArg_2
            return (toGeomClass ret_3)
        )


-----------------------------------------------------------
isGeomEnabled :: Geom -> IO Bool
isGeomEnabled arg_0 = (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- isGeomEnableddGeomIsEnabled marshaledArg_2
            return (toBool ret_3)
        )

-----------------------------------------------------------
setSafeGeomData :: (Typeable a) => Geom -> a -> IO ()
setSafeGeomData body d = setGeomData body (typeOf d, d)

-----------------------------------------------------------
getGeomData :: Geom -> IO a
getGeomData body =
    getRawGeomData body >>= deRefStablePtr . castPtrToStablePtr
setGeomData :: Geom -> a -> IO ()
setGeomData body d =
    newStablePtr d
        >>= \stablePtr -> setRawGeomData body (castStablePtrToPtr stablePtr)

tryGetSafeGeomData :: (Typeable a) => Geom -> IO (Maybe a)
tryGetSafeGeomData body =
    getGeomData body
        >>= \(t, d) ->
            if t == typeOf d then return (Just d) else return Nothing

getSafeGeomData :: (Typeable a) => Geom -> IO a
getSafeGeomData =
    fmap (fromMaybe (error errMsg)) . tryGetSafeGeomData
  where
    errMsg = "Physics.ODE.Geom.getSafeGeomData: invalid type."


-----------------------------------------------------------
getBodyUnsafe :: Geom -> IO Body
getBodyUnsafe arg_0 = (\action_1 -> action_1 arg_0)
        getBodyUnsafedGeomGetBody
