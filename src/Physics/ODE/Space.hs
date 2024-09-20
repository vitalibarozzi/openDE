module Physics.ODE.Space
    (
      module Physics.ODE.Space
    , module Physics.ODE.Raw.Space
    )
where

import Data.Maybe
import Foreign
import Physics.ODE.Raw.Space
import Physics.ODE.Raw.Types
import Control.Monad.IO.Class

-- TODO foreign import ccall unsafe "dQuadTreeSpaceCreate" {--------} c'dQuadTreeSpaceCreate {--}   :: Space -> Int -> IO Geom-- TODO is this type correct?
-- TODO foreign import ccall unsafe "dSpaceSetSublevel" {-----------} c'dSpaceSetSublevel {-----}   :: Space -> Int -> IO ()
-- TODO foreign import ccall unsafe "dSpaceGetSublevel" {-----------} c'dSpaceGetSublevel {-----}   :: Space -> IO Int

-----------------------------------------------------------
{-# INLINE tryGetGeom #-}
tryGetGeom :: (MonadIO m) => Space -> Int -> m (Maybe Geom)
tryGetGeom space nth = 
    liftIO $ do
        num <- getNumGeoms space
        if nth > num - 1
            then return Nothing
            else fmap Just (getGeomUnsafe space nth)

-----------------------------------------------------------
{-# INLINE getGeom #-}
getGeom :: (MonadIO m) => Space -> Int -> m Geom
getGeom space nth =
    liftIO $ do
        fmap (fromMaybe (error msg)) (tryGetGeom space nth)
  where
    msg =
        "Physics.ODE.Space.getGeom: Index ("
            ++ show nth
            ++ ") out of range."

-----------------------------------------------------------
{-# INLINE createSimple #-}
createSimple :: (MonadIO m) => Maybe Space -> m Space
createSimple mspace =
    liftIO $ do
        case mspace of
            Data.Maybe.Just a_1 -> undefined -- c'dCreateSimple a_1
            Data.Maybe.Nothing -> undefined -- c'dCreateSimple nullPtr
        

-----------------------------------------------------------
{-# INLINE createHash #-}
createHash :: (MonadIO m) => Maybe Space -> m Space
createHash arg_0 =
    liftIO $ do
        case arg_0 of
            Data.Maybe.Just a_1 -> undefined -- c'createHashdHashSpaceCreate a_1
            Data.Maybe.Nothing -> undefined -- c'createHashdHashSpaceCreate nullPtr

-----------------------------------------------------------
{-# INLINE destroySpace #-}
destroySpace :: (MonadIO m) => Space -> m ()
destroySpace arg_0 =
    liftIO $ do
        (\action_1 -> action_1 arg_0)
            ( \marshaledArg_2 -> do
                ret_3 <- undefined -- c'destroySpacedSpaceDestroy marshaledArg_2
                case () of
                    () -> return ()
            )

-----------------------------------------------------------
{-# INLINE setLevels #-}
setLevels :: (MonadIO m) => Space -> Int -> Int -> m ()
setLevels arg_0 arg_1 arg_2 =
    liftIO $ do
        (\action_3 -> action_3 arg_0)
            ( \marshaledArg_4 ->
                (\action_5 -> action_5 arg_1)
                    ( \marshaledArg_6 ->
                        (\action_7 -> action_7 arg_2)
                            ( \marshaledArg_8 -> do
                                ret_9 <- undefined -- c'setLevelsdHashSpaceSetLevels marshaledArg_4 marshaledArg_6 marshaledArg_8
                                case () of
                                    () -> return ()
                            )
                    )
            )
{-# INLINE getLevels #-}
getLevels :: (MonadIO m) => Space -> m (Int, Int)
getLevels arg_0 =
    liftIO $ do
        (\action_1 -> action_1 arg_0)
            ( \marshaledArg_2 ->
                alloca
                    ( \marshaledArg_3 ->
                        alloca
                            ( \marshaledArg_4 -> do
                                ret_5 <- undefined -- c'getLevelsdHashSpaceGetLevels marshaledArg_2 marshaledArg_3 marshaledArg_4
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
{-# INLINE setCleanup #-}
setCleanup :: (MonadIO m) => Space -> Bool -> m ()
setCleanup arg_0 arg_1 =
    liftIO $ do
        (\action_2 -> action_2 arg_0)
            ( \marshaledArg_3 ->
                undefined -- (\action_4 -> undefined) -- action_4 (fromBool arg_1))
                    -- ( \marshaledArg_5 -> do
                    --     ret_6 <- undefined -- c'setCleanupdSpaceSetCleanup marshaledArg_3 marshaledArg_5
                     --    case () of
                     --        () -> return ()
                   --  )
            )


{-# INLINE getCleanup #-}
getCleanup :: (MonadIO m) => Space -> m Bool
getCleanup arg_0 =
    liftIO $ do
        (\action_1 -> action_1 arg_0)
            ( \marshaledArg_2 -> do
                ret_3 <- undefined -- c'getCleanupdSpaceGetCleanup marshaledArg_2
                undefined -- return (toBool ret_3)
            )

-----------------------------------------------------------
{-# INLINE add #-}
add :: (MonadIO m) => Space -> Geom -> m ()
add arg_0 arg_1 =
    liftIO $ do
        (\action_2 -> action_2 arg_0)
            ( \marshaledArg_3 ->
                (\action_4 -> action_4 arg_1)
                    ( \marshaledArg_5 -> do
                        ret_6 <- undefined -- c'adddSpaceAdd marshaledArg_3 marshaledArg_5
                        case () of
                            () -> return ()
                    )
            )

-----------------------------------------------------------
{-# INLINE remove #-}
remove :: (MonadIO m) => Space -> Geom -> m ()
remove arg_0 arg_1 =
    liftIO $ do
        (\action_2 -> action_2 arg_0)
            ( \marshaledArg_3 ->
                (\action_4 -> action_4 arg_1)
                    ( \marshaledArg_5 -> do
                        ret_6 <- undefined -- c'removedSpaceRemove marshaledArg_3 marshaledArg_5
                        case () of
                            () -> return ()
                    )
            )

-----------------------------------------------------------
{-# INLINE query #-}
query :: (MonadIO m) => Space -> Geom -> m Bool
query arg_0 arg_1 =
    liftIO $ do
        (\action_2 -> action_2 arg_0)
            ( \marshaledArg_3 ->
                (\action_4 -> action_4 arg_1)
                    ( \marshaledArg_5 -> do
                        ret_6 <- undefined -- c'querydSpaceQuery marshaledArg_3 marshaledArg_5
                        undefined -- return (toBool ret_6)
                    )
            )

-----------------------------------------------------------
{-# INLINE getNumGeoms #-}
getNumGeoms :: (MonadIO m) => Space -> m Int
getNumGeoms arg_0 =
    liftIO $ do
        (\action_1 -> action_1 arg_0)
            ( \marshaledArg_2 -> do
                undefined -- c'getNumGeomsdSpaceGetNumGeoms marshaledArg_2
            )

-----------------------------------------------------------
{-# INLINE getGeomUnsafe #-}
getGeomUnsafe :: (MonadIO m) => Space -> Int -> m Geom
getGeomUnsafe arg_0 arg_1 =
    liftIO $ do
        (\action_2 -> action_2 arg_0)
            ( \marshaledArg_3 ->
                (\action_4 -> action_4 arg_1)
                    ( \marshaledArg_5 -> do
                        undefined -- c'getGeomUnsafedSpaceGetGeom marshaledArg_3 marshaledArg_5
                    )
            )
