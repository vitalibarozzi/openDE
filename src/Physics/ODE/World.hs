module Physics.ODE.World (
    create,
    destroy,
    gravity,
    contactSurfaceLayer,
    setCFM,
    getCFM,
)
where

import Control.Monad.IO.Class
import Data.StateVar
import Foreign
import Physics.ODE.Raw.Types as World
import Physics.ODE.Utilities
import Physics.ODE.Raw.World as World

-----------------------------------------------------------
create :: (MonadIO m) => m World
{-# INLINE create #-}
create =
    liftIO c'createdWorldCreate

-----------------------------------------------------------
destroy :: (MonadIO m) => World -> m ()
{-# INLINE destroy #-}
destroy =
    liftIO . c'destroyWorlddWorldDestroy

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

-----------------------------------------------------------
setCFM :: (MonadIO m) => World -> Float -> m ()
setCFM w f = liftIO (c'worldSetCFM w f)

-----------------------------------------------------------
getCFM :: (MonadIO m) => World -> m Float
getCFM = liftIO . c'worldGetCFM
