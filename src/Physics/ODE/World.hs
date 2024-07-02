module Physics.ODE.World (
    create,
    destroy,
    gravity,
    contactSurfaceLayer,
)
where

import Data.StateVar
import Foreign
import Physics.ODE.Raw.Types as World
import Physics.ODE.Utilities
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
gravity :: World -> StateVar (Float, Float, Float)
{-# INLINE gravity #-}
gravity world =
    StateVar get_ set_
  where
    get_ = allocaArray 4 (\vec -> c'getGravitydWorldGetGravity world vec >> peekVector3 vec)
    set_ (x, y, z) = c'setGravitydWorldSetGravity world x y z

-----------------------------------------------------------
contactSurfaceLayer :: World -> StateVar Float
{-# INLINE contactSurfaceLayer #-}
contactSurfaceLayer world = do
    StateVar get_ set_
  where
    get_ = c'getContactSurfaceLayerdWorldGetContactSurfaceLayer world
    set_ = c'setContactSurfaceLayerdWorldSetContactSurfaceLayer world
