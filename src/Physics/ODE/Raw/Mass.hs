module Physics.ODE.Raw.Mass
    (
    c'massAdd,
    c'adjustdMassAdjust,
    c'massSetBox,
    c'massSetBoxTotal,
    c'massSetCappedCylinder,
    c'massSetCappedCylinderTotal,
    c'massSetCapsule,
    c'massSetCapsuleTotal,
    c'massSetCylinder,
    c'massSetCylinderTotal,
    c'massSetParameters,
    c'massSetSphere,
    c'massSetSphereTotal,
    c'massSetTrimesh,
    c'setZero,
    c'massTranslate,
    )
where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dMassAdd"                     c'massAdd                      :: Ptr MassStruct -> Float -> IO () -- TODO not correct
foreign import ccall unsafe "dMassAdjust"                  c'adjustdMassAdjust            :: Ptr MassStruct -> Float -> IO () -- TODO not correct
foreign import ccall unsafe "dMassSetBox"                  c'massSetBox                  :: Ptr MassStruct -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetBoxTotal"             c'massSetBoxTotal             :: Ptr MassStruct -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCappedCylinder"       c'massSetCappedCylinder       :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCappedCylinderTotal"  c'massSetCappedCylinderTotal  :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCapsule"              c'massSetCapsule              :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCapsuleTotal"         c'massSetCapsuleTotal         :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetCylinder"             c'massSetCylinder             :: Ptr MassStruct -> Float -> Int -> Float  -> Float -> IO ()
foreign import ccall unsafe "dMassSetCylinderTotal"        c'massSetCylinderTotal        :: Ptr MassStruct -> Float -> Int -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetParameters"           c'massSetParameters           :: Ptr MassStruct -> Float -> Float -> Float -> Float ->Float ->Float ->Float ->Float ->Float ->Float ->IO ()
foreign import ccall unsafe "dMassSetSphere"               c'massSetSphere                :: Ptr MassStruct -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetSphereTotal"          c'massSetSphereTotal           :: Ptr MassStruct -> Float -> Float -> IO ()
foreign import ccall unsafe "dMassSetTrimesh"              c'massSetTrimesh              :: Ptr MassStruct -> Float -> Geom -> IO ()
foreign import ccall unsafe "dMassSetZero"                 c'setZero                      :: Ptr MassStruct -> IO ()
foreign import ccall unsafe "dMassTranslate"               c'massTranslate                :: Ptr MassStruct -> Float -> Float -> Float -> IO ()
--  void dMassRotate (dMass *, const dMatrix3 R);
--foreign import ccall unsafe "dMassRotate"   c'massRotate       :: Ptr MassStruct -> Float -> Float -> Float -> IO ()
