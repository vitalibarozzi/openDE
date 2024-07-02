module Physics.ODE.Mass (
    create,
    destroyMass,
    mass,
    setZero,
    adjust,
) where

import Foreign
import Physics.ODE.Raw.Hsc
import Physics.ODE.Raw.Types
import Physics.ODE.Utilities
import Physics.ODE.Raw.Mass

create :: IO Mass
create = mallocForeignPtrBytes sizeOfMass

destroyMass :: Mass -> IO ()
destroyMass = forceFinalization

mass :: Mass -> IO Float
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

adjust :: Mass -> Float -> IO ()
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
