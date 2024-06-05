module Physics.ODE.World where

import Foreign
import Physics.ODE.Raw.World as World
import Physics.ODE.Raw.Types as World
import Control.Exception
import Control.Monad.IO.Class
import Physics.ODE.Raw.Utilities



--createObj :: (MonadIO m) => Object -> m ()
--createObj = \case
    --Sphere Double
    --Box    ()
    --Plane  ()
    --Ball o0 o1 -> do
    --    oo0 <- createObjPrim o0
    --    oo1 <- createObjPrim o1
    --Hinge  Object Object
    --Slider Object Object


--data Object
--    = Sphere Double
--    | Box    ()
--    | Plane  ()
--    | Ball   Object Object
--    | Hinge  Object Object
--    | Slider Object Object



withODE :: (MonadIO m) => (World.World -> IO ()) -> m ()
withODE k = 
    liftIO $ bracket
        World.initODE
        (\() -> World.closeODE)
        (\() -> k =<< create)

create :: IO World
create = do
    ret_0 <- createdWorldCreate
    return (ret_0)

destroyWorld :: World -> IO ()
destroyWorld = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- destroyWorlddWorldDestroy marshaledArg_2
            case () of
                () -> do return ()
        )

setGravity :: World -> ODEreal -> ODEreal -> ODEreal -> IO ()
setGravity = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- setGravitydWorldSetGravity marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> do return ()
                                )
                        )
                )
        )

getGravity :: World -> IO ((ODEreal, ODEreal, ODEreal))
getGravity = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 ->
            allocaArray
                4
                ( \marshaledArg_3 -> do
                    ret_4 <- getGravitydWorldGetGravity marshaledArg_2 marshaledArg_3
                    peekVector3 (marshaledArg_3)
                )
        )

step :: World -> ODEreal -> IO ()
step = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- stepdWorldStep marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )

quickStep :: World -> ODEreal -> IO ()
quickStep = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- quickStepdWorldQuickStep marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )

setContactSurfaceLayer :: World -> ODEreal -> IO ()
setContactSurfaceLayer = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- setContactSurfaceLayerdWorldSetContactSurfaceLayer marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )

getContactSurfaceLayer :: World -> IO ODEreal
getContactSurfaceLayer = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getContactSurfaceLayerdWorldGetContactSurfaceLayer marshaledArg_2
            return (ret_3)
        )
