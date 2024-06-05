module Physics.ODE.Raw.World where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dInitODE2" initODE :: IO ()
foreign import ccall unsafe "dCloseODE" closeODE :: IO ()
foreign import ccall unsafe "dWorldCreate" createdWorldCreate :: IO World
foreign import ccall unsafe "dWorldDestroy" destroyWorlddWorldDestroy :: World -> IO ()
foreign import ccall unsafe "dWorldSetGravity" setGravitydWorldSetGravity :: World -> ODEreal -> ODEreal -> ODEreal -> IO ()
foreign import ccall unsafe "dWorldGetGravity" getGravitydWorldGetGravity :: World -> Ptr ODEreal -> IO ()
foreign import ccall unsafe "dWorldStep" stepdWorldStep :: World -> ODEreal -> IO ()
foreign import ccall unsafe "dWorldQuickStep" quickStepdWorldQuickStep :: World -> ODEreal -> IO ()
foreign import ccall unsafe "dWorldSetContactSurfaceLayer" setContactSurfaceLayerdWorldSetContactSurfaceLayer :: World -> ODEreal -> IO ()
foreign import ccall unsafe "dWorldGetContactSurfaceLayer" getContactSurfaceLayerdWorldGetContactSurfaceLayer :: World -> IO ODEreal
