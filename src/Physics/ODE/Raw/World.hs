module Physics.ODE.Raw.World (
    c'initODE,
    c'closeODE,
    c'createdWorldCreate,
    c'destroyWorlddWorldDestroy,
    c'setGravitydWorldSetGravity,
    c'getGravitydWorldGetGravity,
    c'stepdWorldStep,
    c'quickStepdWorldQuickStep,
    c'setContactSurfaceLayerdWorldSetContactSurfaceLayer,
    c'getContactSurfaceLayerdWorldGetContactSurfaceLayer,
)
where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dInitODE2" c'initODE :: IO ()
foreign import ccall unsafe "dCloseODE" c'closeODE :: IO ()
foreign import ccall unsafe "dWorldCreate" c'createdWorldCreate :: IO World
foreign import ccall unsafe "dWorldDestroy" c'destroyWorlddWorldDestroy :: World -> IO ()
foreign import ccall unsafe "dWorldSetGravity" c'setGravitydWorldSetGravity :: World -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dWorldGetGravity" c'getGravitydWorldGetGravity :: World -> Ptr Float -> IO ()
foreign import ccall unsafe "dWorldStep" c'stepdWorldStep :: World -> Float -> IO ()
foreign import ccall unsafe "dWorldQuickStep" c'quickStepdWorldQuickStep :: World -> Float -> IO ()
foreign import ccall unsafe "dWorldSetContactSurfaceLayer" c'setContactSurfaceLayerdWorldSetContactSurfaceLayer :: World -> Float -> IO ()
foreign import ccall unsafe "dWorldGetContactSurfaceLayer" c'getContactSurfaceLayerdWorldGetContactSurfaceLayer :: World -> IO Float
