module Physics.ODE.Mass (
    create,
    destroyMass,
    mass,
    setZero,
    adjust,
) where

import Foreign
import Physics.ODE.Hsc
import Physics.ODE.Types
import Physics.ODE.Utilities

create :: IO Mass
create = mallocForeignPtrBytes sizeOfMass

destroyMass :: Mass -> IO ()
destroyMass = forceFinalization

mass :: Mass -> IO ODEreal
mass m = withForeignPtr m peekMass

--  void dMassSetZero (dMass *);
setZero :: Mass -> IO ()
setZero = \arg_0 ->
    withForeignPtr
        arg_0
        ( \marshaledArg_1 -> do
            ret_2 <- setZerodMassSetZero marshaledArg_1
            case () of
                () -> do return ()
        )

adjust :: Mass -> ODEreal -> IO ()
adjust = \arg_0 arg_1 ->
    withForeignPtr
        arg_0
        ( \marshaledArg_2 ->
            (\action_3 -> action_3 arg_1)
                ( \marshaledArg_4 -> do
                    ret_5 <- adjustdMassAdjust marshaledArg_2 marshaledArg_4
                    case () of
                        () -> do return ()
                )
        )

foreign import ccall unsafe "dMassAdjust"
    adjustdMassAdjust ::
        Ptr MassStruct ->
        ODEreal ->
        IO ()

--  void dMassTranslate (dMass *, dReal x, dReal y, dReal z);
--  void dMassRotate (dMass *, const dMatrix3 R);
--  void dMassAdd (dMass *a, const dMass *b);
foreign import ccall unsafe "dMassSetZero"
    setZerodMassSetZero ::
        Ptr MassStruct ->
        IO ()

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
