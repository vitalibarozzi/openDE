module Physics.ODE.Collision (
    collide,
    spaceCollide,
) where

import Foreign
import Physics.ODE.Raw.Collision
import Physics.ODE.Raw.Hsc
import Physics.ODE.Raw.Types

-----------------------------------------------------------
type Callback = Geom -> Geom -> IO ()

-----------------------------------------------------------
collide :: Geom -> Geom -> Int -> IO [ContactInfo]
collide geom1 geom2 nElems =
    allocaArray nElems $ \points -> do
        ___ <- cMemset points 0 (sizeOfContactInfo * nElems)
        ret <- rawCollide geom1 geom2 nElems (addressOfGeom points) sizeOfContactInfo
        peekArray ret points
  where
    sizeOfContactInfo = sizeOf (error "bug" :: ContactInfo)

-----------------------------------------------------------
spaceCollide :: Space -> Callback -> IO ()
spaceCollide space callback =
    mkRawCallback (const callback) >>= \rawCallback ->
        cSpaceCollide space nullPtr rawCallback
            >> freeHaskellFunPtr rawCallback
