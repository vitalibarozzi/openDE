module Physics.ODE.World
       (create, destroyWorld, setGravity, getGravity, initODE, closeODE, step,
        quickStep, setContactSurfaceLayer, getContactSurfaceLayer)
       where
import Foreign
import Physics.ODE.Types
import Physics.ODE.Utilities
 
foreign import ccall unsafe "dInitODE2" initODE :: IO ()
foreign import ccall unsafe "dCloseODE" closeODE :: IO ()
create :: IO World
create = do ret_0 <- createdWorldCreate
            return (ret_0)
foreign import ccall unsafe "dWorldCreate" createdWorldCreate :: IO World
destroyWorld :: World -> IO ()
destroyWorld = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- destroyWorlddWorldDestroy marshaledArg_2
                                                                              case () of
                                                                                  () -> do return ())
foreign import ccall unsafe "dWorldDestroy" destroyWorlddWorldDestroy :: World ->
                                                                         IO ()
setGravity :: World -> ODEreal -> ODEreal -> ODEreal -> IO ()
setGravity = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setGravitydWorldSetGravity marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                       case () of
                                                                                                                                                                                                                                                           () -> do return ()))))
foreign import ccall unsafe "dWorldSetGravity" setGravitydWorldSetGravity :: World ->
                                                                             ODEreal ->
                                                                             ODEreal ->
                                                                             ODEreal -> IO ()
getGravity :: World -> IO ((ODEreal, ODEreal, ODEreal))
getGravity = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> allocaArray 4 (\marshaledArg_3 -> do ret_4 <- getGravitydWorldGetGravity marshaledArg_2 marshaledArg_3
                                                                                                              peekVector3 (marshaledArg_3)))
foreign import ccall unsafe "dWorldGetGravity" getGravitydWorldGetGravity :: World ->
                                                                             Ptr ODEreal -> IO ()
step :: World -> ODEreal -> IO ()
step = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 arg_1) (\marshaledArg_5 -> do ret_6 <- stepdWorldStep marshaledArg_3 marshaledArg_5
                                                                                                                              case () of
                                                                                                                                  () -> do return ()))
foreign import ccall unsafe "dWorldStep" stepdWorldStep :: World ->
                                                           ODEreal -> IO ()
quickStep :: World -> ODEreal -> IO ()
quickStep = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 arg_1) (\marshaledArg_5 -> do ret_6 <- quickStepdWorldQuickStep marshaledArg_3 marshaledArg_5
                                                                                                                                   case () of
                                                                                                                                       () -> do return ()))
foreign import ccall unsafe "dWorldQuickStep" quickStepdWorldQuickStep :: World ->
                                                                          ODEreal -> IO ()
setContactSurfaceLayer :: World -> ODEreal -> IO ()
setContactSurfaceLayer = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 arg_1) (\marshaledArg_5 -> do ret_6 <- setContactSurfaceLayerdWorldSetContactSurfaceLayer marshaledArg_3 marshaledArg_5
                                                                                                                                                case () of
                                                                                                                                                    () -> do return ()))
foreign import ccall unsafe "dWorldSetContactSurfaceLayer" setContactSurfaceLayerdWorldSetContactSurfaceLayer :: World ->
                                                                                                                 ODEreal ->
                                                                                                                 IO ()
getContactSurfaceLayer :: World -> IO ODEreal
getContactSurfaceLayer = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getContactSurfaceLayerdWorldGetContactSurfaceLayer marshaledArg_2
                                                                                        return (ret_3))
foreign import ccall unsafe "dWorldGetContactSurfaceLayer" getContactSurfaceLayerdWorldGetContactSurfaceLayer :: World ->
                                                                                                                 IO ODEreal
