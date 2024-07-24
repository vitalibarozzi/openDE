module Physics.ODE.Space where

import Data.Maybe
import Foreign
import Physics.ODE.Raw.Space
import Physics.ODE.Raw.Types

-----------------------------------------------------------
tryGetGeom :: Space -> Int -> IO (Maybe Geom)
tryGetGeom space nth = do
    num <- getNumGeoms space
    if nth > num - 1
        then return Nothing
        else fmap Just (getGeomUnsafe space nth)

-----------------------------------------------------------
getGeom :: Space -> Int -> IO Geom
getGeom space nth =
    fmap (fromMaybe (error msg)) (tryGetGeom space nth)
  where
    msg =
        "Physics.ODE.Space.getGeom: Index ("
            ++ show nth
            ++ ") out of range."
-----------------------------------------------------------
createSimple :: Maybe Space -> IO Space
createSimple arg_0 =
    ( case arg_0 of
        Data.Maybe.Just a_1 -> \action_2 -> action_2 a_1
        Data.Maybe.Nothing -> \action_3 -> action_3 nullPtr
    )
        ( \marshaledArg_4 -> do
            c'createSimpledSimpleSpaceCreate marshaledArg_4
        )
-----------------------------------------------------------
createHash :: Maybe Space -> IO Space
createHash arg_0 =
    ( case arg_0 of
        Data.Maybe.Just a_1 -> \action_2 -> action_2 a_1
        Data.Maybe.Nothing -> \action_3 -> action_3 nullPtr
    )
        ( \marshaledArg_4 -> do
            c'createHashdHashSpaceCreate marshaledArg_4
        )
-----------------------------------------------------------
destroySpace :: Space -> IO ()
destroySpace arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- c'destroySpacedSpaceDestroy marshaledArg_2
            case () of
                () -> return ()
        )
-----------------------------------------------------------
setLevels :: Space -> Int -> Int -> IO ()
setLevels arg_0 arg_1 arg_2 =
    (\action_3 -> action_3 arg_0)
        ( \marshaledArg_4 ->
            (\action_5 -> action_5 arg_1)
                ( \marshaledArg_6 ->
                    (\action_7 -> action_7 arg_2)
                        ( \marshaledArg_8 -> do
                            ret_9 <- c'setLevelsdHashSpaceSetLevels marshaledArg_4 marshaledArg_6 marshaledArg_8
                            case () of
                                () -> return ()
                        )
                )
        )
-----------------------------------------------------------
getLevels :: Space -> IO (Int, Int)
getLevels arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 ->
            alloca
                ( \marshaledArg_3 ->
                    alloca
                        ( \marshaledArg_4 -> do
                            ret_5 <- c'getLevelsdHashSpaceGetLevels marshaledArg_2 marshaledArg_3 marshaledArg_4
                            case ( marshaledArg_3
                                 , marshaledArg_4
                                 ) of
                                ( tuplePart_6
                                    , tuplePart_7
                                    ) -> do
                                        returnVariable_8 <- peek tuplePart_6
                                        returnVariable_10 <- peek tuplePart_7
                                        return
                                            ( returnVariable_8
                                            , returnVariable_10
                                            )
                        )
                )
        )
-----------------------------------------------------------
setCleanup :: Space -> Bool -> IO ()
setCleanup arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 (fromBool arg_1))
                ( \marshaledArg_5 -> do
                    ret_6 <- c'setCleanupdSpaceSetCleanup marshaledArg_3 marshaledArg_5
                    case () of
                        () -> return ()
                )
        )
-----------------------------------------------------------
getCleanup :: Space -> IO Bool
getCleanup arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- c'getCleanupdSpaceGetCleanup marshaledArg_2
            return (toBool ret_3)
        )
-----------------------------------------------------------
add :: Space -> Geom -> IO ()
add arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- c'adddSpaceAdd marshaledArg_3 marshaledArg_5
                    case () of
                        () -> return ()
                )
        )
-----------------------------------------------------------
remove :: Space -> Geom -> IO ()
remove arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- c'removedSpaceRemove marshaledArg_3 marshaledArg_5
                    case () of
                        () -> return ()
                )
        )
-----------------------------------------------------------
query :: Space -> Geom -> IO Bool
query arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- c'querydSpaceQuery marshaledArg_3 marshaledArg_5
                    return (toBool ret_6)
                )
        )
-----------------------------------------------------------
getNumGeoms :: Space -> IO Int
getNumGeoms arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            c'getNumGeomsdSpaceGetNumGeoms marshaledArg_2
        )
-----------------------------------------------------------
getGeomUnsafe :: Space -> Int -> IO Geom
getGeomUnsafe arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    c'getGeomUnsafedSpaceGetGeom marshaledArg_3 marshaledArg_5
                )
        )
