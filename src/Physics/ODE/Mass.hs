module Physics.ODE.Mass (
    create,
    destroy,
    setZero,
    mass,
    add,
    setBox,
    setBoxTotal,
    setCappedCylinder,
    setCappedCylinderTotal,
    setCylinder,
    setCylinderTotal,
    setCapsule,
    setCapsuleTotal,
    setParameters,
    setSphere,
    setSphereTotal,
    setTrimesh,
    translate,
) where

import Foreign
import Physics.ODE.Raw.Types
import Physics.ODE.Utilities
import Physics.ODE.Raw.Mass
import Data.StateVar
import Control.Monad.IO.Class


-----------------------------------------------------------
create :: (MonadIO m) => m Mass
create =
    liftIO $ mallocForeignPtrBytes 136

-----------------------------------------------------------
destroy :: (MonadIO m) => Mass -> m ()
destroy =
    forceFinalization

-----------------------------------------------------------
setZero :: (MonadIO m) => Mass -> m ()
setZero mass_ =
    liftIO $ withForeignPtr mass_ c'setZero

-----------------------------------------------------------
mass :: Mass -> StateVar Float
mass m =
    StateVar get_ set
  where
    get_ = withForeignPtr m (`peekByteOff` 0)
    set newValue = withForeignPtr m (`c'adjustdMassAdjust` newValue)

-----------------------------------------------------------
add :: (MonadIO m) => Mass -> Float -> m ()
add m f =
    liftIO $ withForeignPtr m (`c'massAdd` f)

-----------------------------------------------------------
setBox:: (MonadIO m) => Mass -> Float -> Float -> Float -> Float -> m ()
setBox m x y z w = 
    liftIO $ withForeignPtr m (\m -> c'massSetBox m x y z w)

-----------------------------------------------------------
setBoxTotal:: (MonadIO m) => Mass -> Float -> Float -> Float -> Float -> m ()
setBoxTotal m _ _ _ _ = 
    liftIO $ withForeignPtr m (\mm -> undefined)

-----------------------------------------------------------
setCappedCylinder:: (MonadIO m) => Mass -> Float -> Int -> Float -> Float -> m ()
setCappedCylinder m _ _ _ _ = 
    liftIO $ withForeignPtr m (\mm -> undefined)

-----------------------------------------------------------
setCappedCylinderTotal :: (MonadIO m) => Mass -> Float -> Int -> Float -> Float -> m ()
setCappedCylinderTotal m _ _ _ _= 
    liftIO $ withForeignPtr m (\mm -> undefined)

-----------------------------------------------------------
setCylinder:: (MonadIO m) => Mass -> Float -> Int -> Float  -> Float -> m ()
setCylinder m _ _ _ _= 
    liftIO $ withForeignPtr m (\mm -> undefined)

-----------------------------------------------------------
setCylinderTotal:: (MonadIO m) => Mass -> Float -> Int -> Float -> Float -> m ()
setCylinderTotal m _ _ _ _= 
    liftIO $ withForeignPtr m undefined

-----------------------------------------------------------
setCapsule :: (MonadIO m) => Mass -> Float -> Int -> Float -> Float -> m ()
setCapsule m _ _ _ _= 
    liftIO $ withForeignPtr m undefined

-----------------------------------------------------------
setCapsuleTotal:: (MonadIO m) => Mass -> Float -> Int -> Float -> Float -> m ()
setCapsuleTotal m _ _ _ _ = 
    liftIO $ withForeignPtr m undefined

-----------------------------------------------------------
setSphere :: (MonadIO m) => Mass -> Float -> Float -> m ()
setSphere m _ _ = 
    liftIO $ withForeignPtr m undefined

-----------------------------------------------------------
setSphereTotal :: (MonadIO m) => Mass -> Float -> Float -> m ()
setSphereTotal m _ _ = 
    liftIO $ withForeignPtr m undefined

-----------------------------------------------------------
setTrimesh :: (MonadIO m) => Mass -> Float -> Geom -> m ()
setTrimesh m _ _ = 
    liftIO $ withForeignPtr m undefined

-----------------------------------------------------------
translate :: (MonadIO m) => Mass -> Float -> Float -> Float -> m ()
translate m _ _ _ = 
    liftIO $ withForeignPtr m undefined

-----------------------------------------------------------
setParameters:: (MonadIO m) => Mass -> Float -> Float -> Float -> Float ->Float ->Float ->Float ->Float ->Float ->Float -> m ()
setParameters m _ _ _ _ _ _ _ _ _ _ = 
    liftIO $ withForeignPtr m undefined

