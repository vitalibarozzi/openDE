module Physics.ODE.Raw.Mass
    (
    c'massAdd,
    c'adjustdMassAdjust,
    c'dMassSetBox,
    c'dMassSetBoxTotal,
    c'dMassSetCappedCylinder,
    c'dMassSetCappedCylinderTotal,
    c'dMassSetCapsule,
    c'dMassSetCapsuleTotal,
    c'dMassSetCylinder,
    c'dMassSetCylinderTotal,
    c'dMassSetParameters,
    c'massSetSphere,
    c'massSetSphereTotal,
    c'dMassSetTrimesh,
    c'setZero,
    c'massTranslate,
    )
where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dMassAdd"                     c'massAdd                      :: Ptr MassStruct -> Float -> IO () -- TODO not correct
foreign import ccall unsafe "dMassAdjust"                  c'adjustdMassAdjust            :: Ptr MassStruct -> Float -> IO () -- TODO not correct
foreign import ccall unsafe "dMassSetBox"                  c'dMassSetBox                  :: Ptr MassStruct -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetBoxTotal"             c'dMassSetBoxTotal             :: Ptr MassStruct -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCappedCylinder"       c'dMassSetCappedCylinder       :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCappedCylinderTotal"  c'dMassSetCappedCylinderTotal  :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCapsule"              c'dMassSetCapsule              :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCapsuleTotal"         c'dMassSetCapsuleTotal         :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCylinder"             c'dMassSetCylinder             :: Ptr MassStruct -> Float -> Int -> Float  -> Float -> IO ()
foreign import ccall unsafe "dMassSetCylinderTotal"        c'dMassSetCylinderTotal        :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetParameters"           c'dMassSetParameters           :: Ptr MassStruct -> Float -> Float -> Float -> Float ->Float ->Float ->Float ->Float ->Float ->Float ->IO ()
foreign import ccall unsafe "dMassSetSphere"               c'massSetSphere                :: Ptr MassStruct -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetSphereTotal"          c'massSetSphereTotal           :: Ptr MassStruct -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetTrimesh"              c'dMassSetTrimesh              :: Ptr MassStruct -> Float -> Geom -> IO ()
foreign import ccall unsafe "dMassSetZero"                 c'setZero                      :: Ptr MassStruct -> IO ()
foreign import ccall unsafe "dMassTranslate"               c'massTranslate                :: Ptr MassStruct -> Float -> Float -> Float -> IO ()
--  void dMassRotate (dMass *, const dMatrix3 R);
--foreign import ccall unsafe "dMassRotate"   c'massRotate       :: Ptr MassStruct -> Float -> Float -> Float -> IO ()
