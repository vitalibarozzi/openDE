module Physics.ODE.World (
    create,
    destroy,
    step,
    quickStep,
    gravity,
    contactSurfaceLayer,
)
where

import Data.StateVar
import Foreign
import Physics.ODE.Raw.Types as World
import Physics.ODE.Raw.Utilities
import Physics.ODE.Raw.World as World

-----------------------------------------------------------
create :: IO World
{-# INLINE create #-}
create =
    c'createdWorldCreate

-----------------------------------------------------------
destroy :: World -> IO ()
{-# INLINE destroy #-}
destroy =
    c'destroyWorlddWorldDestroy

-----------------------------------------------------------
step :: World -> ODEreal -> IO ()
{-# INLINE step #-}
step =
    c'stepdWorldStep

-----------------------------------------------------------
quickStep :: World -> ODEreal -> IO ()
{-# INLINE quickStep #-}
quickStep =
    c'quickStepdWorldQuickStep

-----------------------------------------------------------
gravity :: World -> StateVar (ODEreal, ODEreal, ODEreal)
{-# INLINE gravity #-}
gravity world =
    StateVar get_ set_
  where
    get_ = allocaArray 4 (\vec -> c'getGravitydWorldGetGravity world vec >> peekVector3 vec)
    set_ (x, y, z) = c'setGravitydWorldSetGravity world x y z

-----------------------------------------------------------
contactSurfaceLayer :: World -> StateVar ODEreal
{-# INLINE contactSurfaceLayer #-}
contactSurfaceLayer world = do
    StateVar get_ set_
  where
    get_ = c'getContactSurfaceLayerdWorldGetContactSurfaceLayer world
    set_ = c'setContactSurfaceLayerdWorldSetContactSurfaceLayer world
