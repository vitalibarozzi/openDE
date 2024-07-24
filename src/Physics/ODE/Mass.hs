module Physics.ODE.Mass (
    create,
    destroyMass,
    mass,
    setZero,
    adjust,
) where

import Foreign
import Physics.ODE.Raw.Types
import Physics.ODE.Utilities
import Physics.ODE.Raw.Mass



-----------------------------------------------------------
create :: IO Mass
create =
    mallocForeignPtrBytes sizeOfMass
  where
    sizeOfMass = 136

-----------------------------------------------------------
destroyMass :: Mass -> IO ()
destroyMass =
    forceFinalization

-----------------------------------------------------------
-- TODO not a float, there are more things, its a struct
mass :: Mass -> IO Float
mass m =
    withForeignPtr m peekMass
  where
    peekMass ptr =
        peekByteOff ptr 0


-----------------------------------------------------------
--  void dMassSetZero (dMass *);
setZero :: Mass -> IO ()
setZero mass_ =
    withForeignPtr
        mass_
        setZerodMassSetZero

-----------------------------------------------------------
-- TODO not a float, there are more things, its a struct
adjust :: Mass -> Float -> IO ()
adjust mass_ newValue =
    withForeignPtr mass_ (`adjustdMassAdjust` newValue)
