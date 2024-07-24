module Physics.ODE.Raw.World (
    c'initODE,
    c'closeODE,
    c'createdWorldCreate,
    c'destroyWorlddWorldDestroy,
    c'setGravitydWorldSetGravity,
    c'getGravitydWorldGetGravity,
    c'stepdWorldStep,
    c'quickStepdWorldQuickStep,
    c'setContactSurfaceLayerdWorldSetContactSurfaceLayer,
    c'getContactSurfaceLayerdWorldGetContactSurfaceLayer,
    c'worldSetCFM,
    c'worldGetCFM,
)
where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dInitODE2" c'initODE                                                               :: IO ()
foreign import ccall unsafe "dCloseODE" c'closeODE                                                              :: IO ()

foreign import ccall unsafe "dWorldCreate" c'createdWorldCreate                                                 :: IO World
foreign import ccall unsafe "dWorldDestroy" c'destroyWorlddWorldDestroy                                         :: World -> IO ()
foreign import ccall unsafe "dWorldSetGravity" c'setGravitydWorldSetGravity                                     :: World -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dWorldGetGravity" c'getGravitydWorldGetGravity                                     :: World -> Ptr Float -> IO ()
foreign import ccall unsafe "dWorldStep" c'stepdWorldStep                                                       :: World -> Float -> IO ()
foreign import ccall unsafe "dWorldQuickStep" c'quickStepdWorldQuickStep                                        :: World -> Float -> IO ()
foreign import ccall unsafe "dWorldSetContactSurfaceLayer" c'setContactSurfaceLayerdWorldSetContactSurfaceLayer :: World -> Float -> IO ()
foreign import ccall unsafe "dWorldGetContactSurfaceLayer" c'getContactSurfaceLayerdWorldGetContactSurfaceLayer :: World -> IO Float
foreign import ccall unsafe "dWorldSetCFM" c'worldSetCFM :: World -> Float -> IO ()
foreign import ccall unsafe "dWorldGetCFM" c'worldGetCFM :: World -> IO Float

{-
void dWorldSetERP (dWorldID, dReal erp);
dReal dWorldGetERP (dWorldID);

void dWorldSetAutoDisableFlag (dWorldID, int do_auto_disable);
int dWorldGetAutoDisableFlag (dWorldID);

void dWorldSetAutoDisableLinearThreshold (dWorldID, dReal linear_threshold);
dReal dWorldGetAutoDisableLinearThreshold (dWorldID);

void dWorldSetAutoDisableAngularThreshold (dWorldID, dReal angular_threshold);
dReal dWorldGetAutoDisableAngularThreshold (dWorldID);

void dWorldSetAutoDisableSteps (dWorldID, int steps);
int dWorldGetAutoDisableSteps (dWorldID);

void dWorldSetAutoDisableTime (dWorldID, dReal time);
dReal dWorldGetAutoDisableTime (dWorldID);

void dWorldImpulseToForce (dWorldID, dReal stepsize, dReal ix, dReal iy, dReal iz, dVector3 force);

void dWorldSetQuickStepNumIterations (dWorldID, int num);
int dWorldGetQuickStepNumIterations (dWorldID);

void dWorldSetQuickStepW (WorldID, dReal over_relaxation);
dReal dWorldGetQuickStepW (dWorldID);

dReal dWorldGetLinearDamping (dWorldID);
dReal dWorldGetAngularDamping (dWorldID);
void dWorldSetLinearDamping (dWorldID, dReal scale);
void dWorldSetAngularDamping (dWorldID, dReal scale);
void dWorldSetDamping (dWorldID, dReal linear_scale, dReal angular_scale); 
dReal dWorldGetLinearDampingThreshold (dWorldID);
dReal dWorldGetAngularDampingThreshold (dWorldID);
void dWorldSetLinearDampingThreshold (dWorldID, dReal threshold);
void dWorldSetAngularDampingThreshold (dWorldID, dReal threshold);

dReal dWorldGetMaxAngularSpeed (dWorldID);
void dWorldSetMaxAngularSpeed (dWorldID, dReal max_speed);

void  dWorldSetContactMaxCorrectingVel (dWorldID, dReal vel); 
dReal dWorldGetContactMaxCorrectingVel (dWorldID);

void  dWorldSetContactSurfaceLayer (dWorldID, dReal depth); 
dReal dWorldGetContactSurfaceLayer (dWorldID);

const char* dGetConfiguration ();

Returns the specific ODE build configuration as a string of tokens. The string can be parsed in a similar way to the OpenGL extension mechanism, the naming convention should be familiar too. The following extensions are reported:

    ODE
    ODE_single_precision
    ODE_double_precision
    ODE_EXT_no_debug
    ODE_EXT_trimesh
    ODE_EXT_opcode
    ODE_EXT_gimpact
    ODE_EXT_malloc_not_alloca
    ODE_OPC_16bit_indices
    ODE_OPC_new_collider

int dCheckConfiguration ( const char* token );

-}

--    AutoDisableFlag = disabled
--    AutoDisableLinearThreshold = 0.01
--    AutoDisableAngularThreshold = 0.01
--    AutoDisableSteps = 10
--    AutoDisableTime = 0
