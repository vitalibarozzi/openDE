module Physics.ODE.Collision (
    collide,
    spaceCollide,
    infinity
) where

import Foreign
import Physics.ODE.Raw.Collision
import Physics.ODE.Raw.Hsc
import Physics.ODE.Raw.Types
import Control.Monad.IO.Class

-- TODO remove it
infinity :: Float
infinity = cInfinity

-----------------------------------------------------------
collide :: (MonadIO m) => Geom -> Geom -> Int -> m [ContactInfo]
{-# INLINE collide #-}
collide geom1 geom2 nElems =
    liftIO $ allocaArray nElems $ \points -> do
        ___ <- cMemset points 0 (sizeOfContactInfo * nElems)
        ret <- rawCollide geom1 geom2 nElems (addressOfGeom points) sizeOfContactInfo
        peekArray ret points
  where
    sizeOfContactInfo = sizeOf (error "bug" :: ContactInfo)

-----------------------------------------------------------
spaceCollide :: (MonadIO m) => Space -> (Geom -> Geom -> IO ()) -> m ()
{-# INLINE spaceCollide #-}
spaceCollide space callback =
    liftIO $ mkRawCallback (const callback) >>= \rawCallback ->
        cSpaceCollide space nullPtr rawCallback
            >> freeHaskellFunPtr rawCallback

