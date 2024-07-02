module Physics.ODE.Raw.Mass where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dMassAdjust" adjustdMassAdjust :: Ptr MassStruct -> Float -> IO ()
foreign import ccall unsafe "dMassSetZero" setZerodMassSetZero :: Ptr MassStruct -> IO ()


--  void dMassTranslate (dMass *, dReal x, dReal y, dReal z);
--  void dMassRotate (dMass *, const dMatrix3 R);
--  void dMassAdd (dMass *a, const dMass *b);
--  void dMassSetParameters (dMass *, dReal themass,
--                           dReal cgx, dReal cgy, dReal cgz,
--                           dReal I11, dReal I22, dReal I33,
--                           dReal I12, dReal I13, dReal I23);
--  void dMassSetSphere (dMass *, dReal density, dReal radius);
--  void dMassSetSphereTotal (dMass *, dReal total_mass, dReal radius);
--  void dMassSetCappedCylinder (dMass *, dReal density, int direction,
--                               dReal radius, dReal length);
--  void dMassSetCappedCylinderTotal (dMass *, dReal total_mass, int direction,
--                                    dReal radius, dReal length);
--  void dMassSetCylinder (dMass *, dReal density, int direction,
--                         dReal radius, dReal length);
--  void dMassSetCylinderTotal (dMass *, dReal total_mass, int direction,
--                              dReal radius, dReal length);
--  void dMassSetBox (dMass *, dReal density,
--                    dReal lx, dReal ly, dReal lz);
--  void dMassSetBoxTotal (dMass *, dReal total_mass,
--                         dReal lx, dReal ly, dReal lz);
--  void dMassAdjust (dMass *, dReal newmass);
