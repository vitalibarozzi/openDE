module Physics.ODE.Raw.Body where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dBodyGetJoint" getJointdBodyGetJoint                                         :: Body -> Int -> IO Joint
foreign import ccall unsafe "dBodySetGravityMode" setGravityModedBodySetGravityMode                       :: Body -> Int -> IO ()
foreign import ccall unsafe "dBodyGetMass" cGetMass                                                       :: Ptr BodyStruct -> Ptr MassStruct -> IO ()
foreign import ccall unsafe "dBodyGetGravityMode" getGravityModedBodyGetGravityMode                       :: Body -> IO Int
foreign import ccall unsafe "dBodySetQuaternion" setBodyQuaterniondBodySetQuaternion                      :: Body -> Ptr Float -> IO ()
foreign import ccall unsafe "dBodyCreate" createdBodyCreate                                               :: World -> IO Body
foreign import ccall unsafe "dBodySetData" setRawBodyData                                                 :: Ptr BodyStruct -> Ptr a -> IO ()
foreign import ccall unsafe "dBodyGetData" getRawBodyData                                                 :: Ptr BodyStruct -> IO (Ptr a)
foreign import ccall unsafe "dBodyDestroy" destroyBodydBodyDestroy                                        :: Body -> IO ()
foreign import ccall unsafe "dBodySetPosition" setBodyPositiondBodySetPosition                            :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetPosition" getBodyPositiondBodyGetPosition                            :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodyGetQuaternion" getBodyQuaterniondBodyGetQuaternion                      :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodySetRotation" setBodyRotationdBodySetRotation                            :: Body -> Matrix3 -> IO ()
foreign import ccall unsafe "dBodyGetRotation" getBodyRotationdBodyGetRotation                            :: Body -> IO Matrix3
foreign import ccall unsafe "dBodySetLinearVel" setLinearVeldBodySetLinearVel                             :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetLinearVel" getLinearVeldBodyGetLinearVel                             :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodySetAngularVel" setAngularVeldBodySetAngularVel                          :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetAngularVel" getAngularVeldBodyGetAngularVel                          :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodySetMass" setMassdBodySetMass                                            :: Body -> Ptr MassStruct -> IO ()
foreign import ccall unsafe "dBodyAddForce" addForcedBodyAddForce                                         :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodySetForce" setForcedBodySetForce                                         :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetForce" getForcedBodyGetForce                                         :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodyAddTorque" addTorquedBodyAddTorque                                      :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodySetTorque" setTorquedBodySetTorque                                      :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetTorque" getTorquedBodyGetTorque                                      :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodyEnable" enableBodydBodyEnable                                           :: Body -> IO ()
foreign import ccall unsafe "dBodyDisable" disableBodydBodyDisable                                        :: Body -> IO ()
foreign import ccall unsafe "dBodyIsEnabled" isBodyEnableddBodyIsEnabled                                  :: Body -> IO Int
foreign import ccall unsafe "dBodySetFiniteRotationMode" setFiniteRotationMode_dBodySetFiniteRotationMode :: Body -> Int -> IO ()
foreign import ccall unsafe "dBodyGetFiniteRotationMode" getFiniteRotationMode_dBodyGetFiniteRotationMode :: Body -> IO Int
foreign import ccall unsafe "dBodySetFiniteRotationAxis" dBodySetFiniteRotationAxis                       :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetFiniteRotationAxis" dBodyGetFiniteRotationAxis                       :: Body -> Ptr Float -> IO ()
foreign import ccall unsafe "dBodyGetNumJoints" dBodyGetNumJoints                                         :: Body -> IO Int

{-

void dBodySetMass (dBodyID, const dMass *mass);
void dBodyGetMass (dBodyID, dMass *mass);

void dBodyAddForce            (dBodyID, dReal fx, dReal fy, dReal fz);
void dBodyAddTorque           (dBodyID, dReal fx, dReal fy, dReal fz);
void dBodyAddRelForce         (dBodyID, dReal fx, dReal fy, dReal fz);
void dBodyAddRelTorque        (dBodyID, dReal fx, dReal fy, dReal fz);
void dBodyAddForceAtPos       (dBodyID, dReal fx, dReal fy, dReal fz, dReal px, dReal py, dReal pz);
void dBodyAddForceAtRelPos    (dBodyID, dReal fx, dReal fy, dReal fz, dReal px, dReal py, dReal pz);
void dBodyAddRelForceAtPos    (dBodyID, dReal fx, dReal fy, dReal fz, dReal px, dReal py, dReal pz);
void dBodyAddRelForceAtRelPos (dBodyID, dReal fx, dReal fy, dReal fz, dReal px, dReal py, dReal pz);

const dReal * dBodyGetForce (dBodyID);
const dReal * dBodyGetTorque (dBodyID);

void dBodySetForce  (dBodyID b, dReal x, dReal y, dReal z);
void dBodySetTorque (dBodyID b, dReal x, dReal y, dReal z);

void dBodySetDynamic (dBodyID);
void dBodySetKinematic (dBodyID);
int dBodyIsKinematic (dBodyID);

void dBodyGetRelPointPos (dBodyID, dReal px, dReal py, dReal pz, dVector3 result);
void dBodyGetRelPointVel (dBodyID, dReal px, dReal py, dReal pz, dVector3 result);
void dBodyGetPointVel    (dBodyID, dReal px, dReal py, dReal pz, dVector3 result);

void dBodyGetPosRelPoint (dBodyID, dReal px, dReal py, dReal pz, dVector3 result);

void dBodyVectorToWorld (dBodyID, dReal px, dReal py, dReal pz, dVector3 result);
void dBodyVectorFromWorld (dBodyID, dReal px, dReal py, dReal pz, dVector3 result);

void dBodyEnable (dBodyID);
void dBodyDisable (dBodyID);

int dBodyIsEnabled (dBodyID);

void dBodySetAutoDisableFlag (dBodyID, int do_auto_disable);
int dBodyGetAutoDisableFlag (dBodyID);

void dBodySetAutoDisableLinearThreshold (dBodyID, dReal linear_threshold);
dReal dBodyGetAutoDisableLinearThreshold (dBodyID);

void dBodySetAutoDisableAngularThreshold (dBodyID, dReal angular_threshold);
dReal dBodyGetAutoDisableAngularThreshold (dBodyID);

void dBodySetAutoDisableSteps (dBodyID, int steps);
int dBodyGetAutoDisableSteps (dBodyID);

void dBodySetAutoDisableTime (dBodyID, dReal time);
dReal dBodyGetAutoDisableTime (dBodyID);

void dBodySetAutoDisableAverageSamplesCount (dBodyID, unsigned int average_samples_count);
int dBodyGetAutoDisableAverageSamplesCount (dBodyID);

void dBodySetAutoDisableDefaults (dBodyID);

void dBodySetMovedCallback (dBodyID, void (*callback)(dBodyID));

dReal dBodyGetLinearDamping (dBodyID);
dReal dBodyGetAngularDamping (dBodyID);
void dBodySetLinearDamping (dBodyID, dReal scale);
void dBodySetAngularDamping (dBodyID, dReal scale);

void dBodySetDamping (dBodyID, dReal linear_scale, dReal angular_scale);

dReal dBodyGetLinearDampingThreshold (dBodyID);
dReal dBodyGetAngularDampingThreshold (dBodyID);
void dBodySetLinearDampingThreshold (dBodyID, dReal threshold);
void dBodySetAngularDampingThreshold (dBodyID, dReal threshold);

void dBodySetDampingDefaults (dBodyID);

dReal dBodyGetMaxAngularSpeed (dBodyID);
void dBodySetMaxAngularSpeed (dBodyID, dReal max_speed);

void dBodySetData (dBodyID, void *data);
void *dBodyGetData (dBodyID);

void dBodySetFiniteRotationMode (dBodyID, int mode);

int dBodyGetFiniteRotationMode (dBodyID);

void dBodySetFiniteRotationAxis (dBodyID, dReal x, dReal y, dReal z);

void dBodyGetFiniteRotationAxis (dBodyID, dVector3 result);

int dBodyGetNumJoints (dBodyID);

dJointID dBodyGetJoint (dBodyID, int index);

dWorldID dBodyGetWorld (dBodyID);

void dBodySetGravityMode (dBodyID b, int mode);
int dBodyGetGravityMode (dBodyID b);

dGeomID dBodyGetFirstGeom (dBodyID);
dGeomID dBodyGetNextGeom (dGeomID);


-}
