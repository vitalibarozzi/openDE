module Physics.ODE.Raw.Space (
    createSimple,
    createHash,
    destroySpace,
    setLevels,
    getLevels,
    setCleanup,
    getCleanup,
    add,
    remove,
    query,
    getNumGeoms,
    getGeomUnsafe,
    tryGetGeom,
    getGeom,
)
where

import Data.Maybe
import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dSimpleSpaceCreate" createSimpledSimpleSpaceCreate :: Space -> IO Space
foreign import ccall unsafe "dHashSpaceCreate" createHashdHashSpaceCreate :: Space -> IO Space
foreign import ccall unsafe "dSpaceDestroy" destroySpacedSpaceDestroy :: Space -> IO ()
foreign import ccall unsafe "dHashSpaceSetLevels" setLevelsdHashSpaceSetLevels :: Space -> Int -> Int -> IO ()
foreign import ccall unsafe "dHashSpaceGetLevels" getLevelsdHashSpaceGetLevels :: Space -> Ptr Int -> Ptr Int -> IO ()
foreign import ccall unsafe "dSpaceSetCleanup" setCleanupdSpaceSetCleanup :: Space -> Int -> IO ()
foreign import ccall unsafe "dSpaceGetCleanup" getCleanupdSpaceGetCleanup :: Space -> IO Int
foreign import ccall unsafe "dSpaceAdd" adddSpaceAdd :: Space -> Geom -> IO ()
foreign import ccall unsafe "dSpaceRemove" removedSpaceRemove :: Space -> Geom -> IO ()
foreign import ccall unsafe "dSpaceQuery" querydSpaceQuery :: Space -> Geom -> IO Int
foreign import ccall unsafe "dSpaceGetNumGeoms" getNumGeomsdSpaceGetNumGeoms :: Space -> IO Int
foreign import ccall unsafe "dSpaceGetGeom" getGeomUnsafedSpaceGetGeom :: Space -> Int -> IO Geom

tryGetGeom :: Space -> Int -> IO (Maybe Geom)
tryGetGeom space nth =
    do
        num <- getNumGeoms space
        if nth > num - 1
            then return Nothing
            else fmap Just (getGeomUnsafe space nth)

getGeom :: Space -> Int -> IO Geom
getGeom space nth =
    fmap (fromMaybe (error msg)) (tryGetGeom space nth)
  where
    msg =
        "Physics.ODE.Space.getGeom: Index ("
            ++ show nth
            ++ ") out of range."
createSimple :: Maybe Space -> IO Space
createSimple = \arg_0 ->
    ( case arg_0 of
        Data.Maybe.Just a_1 -> \action_2 -> action_2 a_1
        Data.Maybe.Nothing -> \action_3 -> action_3 nullPtr
    )
        ( \marshaledArg_4 -> do
            ret_5 <- createSimpledSimpleSpaceCreate marshaledArg_4
            return (ret_5)
        )
createHash :: Maybe Space -> IO Space
createHash = \arg_0 ->
    ( case arg_0 of
        Data.Maybe.Just a_1 -> \action_2 -> action_2 a_1
        Data.Maybe.Nothing -> \action_3 -> action_3 nullPtr
    )
        ( \marshaledArg_4 -> do
            ret_5 <- createHashdHashSpaceCreate marshaledArg_4
            return (ret_5)
        )
destroySpace :: Space -> IO ()
destroySpace = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- destroySpacedSpaceDestroy marshaledArg_2
            case () of
                () -> do return ()
        )
setLevels :: Space -> Int -> Int -> IO ()
setLevels = \arg_0 arg_1 arg_2 ->
    (\action_3 -> action_3 arg_0)
        ( \marshaledArg_4 ->
            (\action_5 -> action_5 arg_1)
                ( \marshaledArg_6 ->
                    (\action_7 -> action_7 arg_2)
                        ( \marshaledArg_8 -> do
                            ret_9 <- setLevelsdHashSpaceSetLevels marshaledArg_4 marshaledArg_6 marshaledArg_8
                            case () of
                                () -> do return ()
                        )
                )
        )
getLevels :: Space -> IO ((Int, Int))
getLevels = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 ->
            alloca
                ( \marshaledArg_3 ->
                    alloca
                        ( \marshaledArg_4 -> do
                            ret_5 <- getLevelsdHashSpaceGetLevels marshaledArg_2 marshaledArg_3 marshaledArg_4
                            case ( marshaledArg_3
                                 , marshaledArg_4
                                 ) of
                                ( tuplePart_6
                                    , tuplePart_7
                                    ) -> do
                                        returnVariable_8 <- do
                                            n_9 <- peek tuplePart_6
                                            return n_9
                                        returnVariable_10 <- do
                                            n_11 <- peek tuplePart_7
                                            return n_11
                                        return
                                            ( returnVariable_8
                                            , returnVariable_10
                                            )
                        )
                )
        )
setCleanup :: Space -> Bool -> IO ()
setCleanup = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 (fromBool arg_1))
                ( \marshaledArg_5 -> do
                    ret_6 <- setCleanupdSpaceSetCleanup marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )
getCleanup :: Space -> IO Bool
getCleanup = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getCleanupdSpaceGetCleanup marshaledArg_2
            return (toBool (ret_3))
        )
add :: Space -> Geom -> IO ()
add = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- adddSpaceAdd marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )
remove :: Space -> Geom -> IO ()
remove = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- removedSpaceRemove marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )
query :: Space -> Geom -> IO Bool
query = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- querydSpaceQuery marshaledArg_3 marshaledArg_5
                    return (toBool (ret_6))
                )
        )
getNumGeoms :: Space -> IO Int
getNumGeoms = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getNumGeomsdSpaceGetNumGeoms marshaledArg_2
            return (ret_3)
        )
getGeomUnsafe :: Space -> Int -> IO Geom
getGeomUnsafe = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- getGeomUnsafedSpaceGetGeom marshaledArg_3 marshaledArg_5
                    return (ret_6)
                )
        )
