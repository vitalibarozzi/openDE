module Physics.ODE.Raw.Joint where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dAreConnected" c'areConnecteddAreConnected                            :: Body -> Body -> IO Int
foreign import ccall unsafe "dAreConnectedExcluding" c'areConnectedExcludingdAreConnectedExcluding :: Body -> Body -> Int -> IO Int
foreign import ccall unsafe "dJointAttach" c'attachdJointAttach                                    :: Joint -> Body -> Body -> IO ()
foreign import ccall unsafe "dJointCreateBall" c'createBalldJointCreateBall                        :: World -> JointGroup -> IO Joint
foreign import ccall unsafe "dJointCreateContact" c'createContactdJointCreateContact               :: World -> JointGroup -> Ptr ContactInfo -> IO Joint
foreign import ccall unsafe "dJointCreateHinge" c'createHingedJointCreateHinge                     :: World -> JointGroup -> IO Joint
foreign import ccall unsafe "dJointCreateSlider" c'createSliderdJointCreateSlider                  :: World -> JointGroup -> IO Joint
foreign import ccall unsafe "dJointDestroy" c'destroyJointdJointDestroy                            :: Joint -> IO ()
foreign import ccall unsafe "dJointDisable" c'jointDisable                                         :: Joint -> IO ()
foreign import ccall unsafe "dJointEnable" c'jointEnable                                           :: Joint -> IO ()
foreign import ccall unsafe "dJointGetBallAnchor" c'getBallAnchordJointGetBallAnchor               :: Joint -> Ptr Float -> IO ()
foreign import ccall unsafe "dJointGetBody" c'getBodydJointGetBody                                 :: Joint -> Int -> IO Body
foreign import ccall unsafe "dJointGetData" c'getRawJointData                                      :: Ptr JointStruct -> IO (Ptr a)
foreign import ccall unsafe "dJointGetType" c'getTypedJointGetType                                 :: Joint -> IO Int
foreign import ccall unsafe "dJointGroupCreate" c'createGroup'dJointGroupCreate                    :: Int -> IO JointGroup
foreign import ccall unsafe "dJointGroupDestroy" c'destroyGroupdJointGroupDestroy                  :: JointGroup -> IO ()
foreign import ccall unsafe "dJointGroupEmpty" c'emptyGroupdJointGroupEmpty                        :: JointGroup -> IO ()
foreign import ccall unsafe "dJointIsEnabled" c'jointIsEnabled                                     :: Joint -> IO Bool
foreign import ccall unsafe "dJointSetBallAnchor" c'setBallAnchordJointSetBallAnchor               :: Joint -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dJointSetData" c'setRawJointData                                      :: Ptr JointStruct -> Ptr a -> IO ()
foreign import ccall unsafe "dJointSetHingeAnchor" c'setHingeAnchordJointSetHingeAnchor            :: Joint -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dJointSetHingeAxis" c'setHingeAxisdJointSetHingeAxis                  :: Joint -> Float -> Float -> Float -> IO ()

{-
dJointID dJointCreateBall (dWorldID, dJointGroupID);
dJointID dJointCreateHinge (dWorldID, dJointGroupID);
dJointID dJointCreateSlider (dWorldID, dJointGroupID);
dJointID dJointCreateContact (dWorldID, dJointGroupID, const dContact *);
dJointID dJointCreateUniversal (dWorldID, dJointGroupID);
dJointID dJointCreateHinge2 (dWorldID, dJointGroupID);
dJointID dJointCreatePR (dWorldID, dJointGroupID);
dJointID dJointCreatePU (dWorldID, dJointGroupID);
dJointID dJointCreatePiston (dWorldID, dJointGroupID);
dJointID dJointCreateFixed (dWorldID, dJointGroupID);
dJointID dJointCreateAMotor (dWorldID, dJointGroupID);
dJointID dJointCreateLMotor (dWorldID, dJointGroupID);
dJointID dJointCreatePlane2D (dWorldID, dJointGroupID);
dJointID dJointCreateDBall (dWorldID, dJointGroupID);
dJointID dJointCreateDHinge (dWorldID, dJointGroupID);
dJointID dJointCreateTransmission (dWorldID, dJointGroupID);
void dJointDestroy (dJointID);

dJointGroupID dJointGroupCreate (int max_size);
void dJointGroupDestroy (dJointGroupID);
void dJointGroupEmpty (dJointGroupID);
void dJointAttach (dJointID, dBodyID body1, dBodyID body2);
void dJointEnable (dJointID);
void dJointDisable (dJointID);
int dJointIsEnabled (dJointID);
void dJointSetData (dJointID, void *data);
void *dJointGetData (dJointID);
int dJointGetType (dJointID);
dBodyID dJointGetBody (dJointID, int index);
int dAreConnected (dBodyID, dBodyID);
int dAreConnectedExcluding (dBodyID, dBodyID, int joint_type);
void dJointSetFeedback (dJointID, dJointFeedback *);
dJointFeedback *dJointGetFeedback (dJointID);
typedef struct dJointFeedback {
    dVector3 f1; // force that joint applies to body 1
    dVector3 t1; // torque that joint applies to body 1
    dVector3 f2; // force that joint applies to body 2
    dVector3 t2; // torque that joint applies to body 2
} dJointFeedback;

void dJointSetBallAnchor (dJointID, dReal x, dReal y, dReal z);

void dJointGetBallAnchor (dJointID, dVector3 result);

void dJointGetBallAnchor2 (dJointID, dVector3 result);

void dJointSetHingeAnchor (dJointID, dReal x, dReal y, dReal z);

void dJointSetHingeAxis (dJointID, dReal x, dReal y, dReal z);

void dJointGetHingeAnchor (dJointID, dVector3 result);

void dJointGetHingeAnchor2 (dJointID, dVector3 result);

void dJointGetHingeAxis (dJointID, dVector3 result);

dReal dJointGetHingeAngle (dJointID);
dReal dJointGetHingeAngleRate (dJointID);

void dJointGetSliderAxis (dJointID, dVector3 result);

dReal dJointGetSliderPosition (dJointID);
dReal dJointGetSliderPositionRate (dJointID);

void dJointSetUniversalAnchor (dJointID, dReal x, dReal y, dReal z);

void dJointSetUniversalAxis1 (dJointID, dReal x, dReal y, dReal z);
void dJointSetUniversalAxis2 (dJointID, dReal x, dReal y, dReal z);

void dJointGetUniversalAnchor (dJointID, dVector3 result);

void dJointGetUniversalAnchor2 (dJointID, dVector3 result);

void dJointGetUniversalAxis1 (dJointID, dVector3 result);
void dJointGetUniversalAxis2 (dJointID, dVector3 result);

dReal dJointGetUniversalAngle1 (dJointID);
dReal dJointGetUniversalAngle2 (dJointID);
dReal dJointGetUniversalAngles (dJointID, dReal *angle1, dReal *angle2);
dReal dJointGetUniversalAngle1Rate (dJointID);
dReal dJointGetUniversalAngle2Rate (dJointID);

void dJointSetHinge2Anchor (dJointID, dReal x, dReal y, dReal z);

void dJointSetHinge2Axis1 (dJointID, dReal x, dReal y, dReal z);
void dJointSetHinge2Axis2 (dJointID, dReal x, dReal y, dReal z);

void dJointGetHinge2Anchor (dJointID, dVector3 result);

void dJointGetHinge2Anchor2 (dJointID, dVector3 result);

void dJointGetHinge2Axis1 (dJointID, dVector3 result);
void dJointGetHinge2Axis2 (dJointID, dVector3 result);

dReal dJointGetHinge2Angle1 (dJointID);
dReal dJointGetHinge2Angle1Rate (dJointID);
dReal dJointGetHinge2Angle2Rate (dJointID);

void dJointSetPRAxis1 (dJointID, dReal x, dReal y, dReal z);
void dJointGetPRAxis1 (dJointID, dVector3 result);

void dJointSetPRAxis2 (dJointID, dReal x, dReal y, dReal z);
void dJointGetPRAxis2 (dJointID, dVector3 result);

void dJointSetPRAnchor (dJointID, dReal x, dReal y, dReal z);
void dJointGetPRAnchor (dJointID, dVector3 result);

dReal dJointGetPRPosition (dJointID);

dReal dJointGetPUPosition (dJointID);

dReal dJointGetPUPositionRate (dJointID);

void dJointSetPUAnchor (dJointID, dReal x, dReal y, dReal z);
void dJointGetPUAnchor (dJointID, dVector3 result);

void dJointSetPUAnchorDelta (dJointID, dReal x, dReal y, dReal z, dReal dx, dReal dy, dReal dz);

void dJointSetPUAxis1 (dJointID, dReal x, dReal y, dReal z);
void dJointGetPUAxis1 (dJointID, dVector3 result);
void dJointSetPUAxis2 (dJointID, dReal x, dReal y, dReal z);
void dJointGetPUAxis2 (dJointID, dVector3 result);

void dJointSetPUAxis3 (dJointID, dReal x, dReal y, dReal z);
void dJointGetPUAxis3 (dJointID, dVector3 result);

void dJointSetPUAxisP (dJointID, dReal x, dReal y, dReal z);
void dJointGetPUAxisP (dJointID, dVector3 result);

void dJointGetPUAngles (dJointID, dReal *angle1, dReal *angle2);
dReal dJointGetPUAngle1 (dJointID);
dReal dJointGetPUAngle2 (dJointID);
dReal dJointGetPUAngle1Rate (dJointID);
dReal dJointGetPUAngle2Rate (dJointID);

void dJointSetPistonAnchor (dJointID, dReal x, dReal y, dReal z);
void dJointGetPistonAnchor (dJointID, dVector3 result);

void dJointGetPistonAnchor2 (dJointID, dVector3 result);

void dJointSetPistonAxis (dJointID, dReal x, dReal y, dReal z);
void dJointGetPistonAxis (dJointID, dVector3 result);

void dJointSetPistonAxisDelta (dJointID j, dReal x, dReal y, dReal z, dReal dx, dReal dy, dReal dz);
dReal dJointGetPistonPosition (dJointID);
dReal dJointGetPistonPositionRate (dJointID);

dReal dJointGetPistonAngle (dJointID);
dReal dJointGetPistonAngleRate (dJointID);

void dJointAddPistonForce (dJointID j, dReal force)

void dJointSetFixed (dJointID);

void dJointSetAMotorMode (dJointID, int mode);
int dJointGetAMotorMode (dJointID);
dAMotorUser 	The AMotor axes and joint angle settings are entirely controlled by the user. This is the default mode.
dAMotorEuler 	Euler angles are automatically computed. The axis a1 is also automatically computed. The AMotor axes must be set correctly when in this mode, as described below. When this mode is initially set the current relative orientations of the bodies will correspond to all euler angles at zero.

void dJointSetAMotorNumAxes (dJointID, int num);
int dJointGetAMotorNumAxes (dJointID);

void dJointSetAMotorAxis (dJointID, int anum, int rel, dReal x, dReal y, dReal z);
void dJointGetAMotorAxis (dJointID, int anum, dVector3 result);
int dJointGetAMotorAxisRel (dJointID, int anum);

void dJointSetAMotorAngle (dJointID, int anum, dReal angle);

dReal dJointGetAMotorAngle (dJointID, int anum);

dReal dJointGetAMotorAngleRate (dJointID, int anum);

void dJointSetLMotorNumAxes (dJointID, int num);
int dJointGetLMotorNumAxes (dJointID);

void dJointSetLMotorAxis (dJointID, int anum, int rel, dReal x, dReal y, dReal z);
void dJointGetLMotorAxis (dJointID, int anum, dVector3 result);

void dJointSetDBallAnchor1(dJointID, dReal x, dReal y, dReal z);
void dJointSetDBallAnchor2(dJointID, dReal x, dReal y, dReal z);
void dJointGetDBallAnchor1(dJointID, dVector3 result);
void dJointGetDBallAnchor2(dJointID, dVector3 result);

dReal dJointGetDBallDistance(dJointID);

void dJointSetDBallDistance(dJointID, dReal dist);

void dJointSetDHingeAxis(dJointID, dReal x, dReal y, dReal z);
void dJointGetDHingeAxis(dJointID, dVector3 result);

void dJointSetDHingeAnchor1(dJointID, dReal x, dReal y, dReal z);
void dJointSetDHingeAnchor2(dJointID, dReal x, dReal y, dReal z);
void dJointGetDHingeAnchor1(dJointID, dVector3 result);
void dJointGetDHingeAnchor2(dJointID, dVector3 result);

dReal dJointGetDHingeDistance(dJointID);

void dJointSetTransmissionMode( dJointID j, int mode );
int dJointGetTransmissionMode( dJointID j );

dTransmissionIntersectingAxes
dTransmissionParallelAxes
dTransmissionChainDrive

void dJointGetTransmissionContactPoint1(dJointID, dVector3 result);
void dJointGetTransmissionContactPoint2(dJointID, dVector3 result);

void dJointSetTransmissionAxis1(dJointID, dReal x, dReal y, dReal z);
void dJointSetTransmissionAxis2(dJointID, dReal x, dReal y, dReal z);
void dJointGetTransmissionAxis1(dJointID, dVector3 result);
void dJointGetTransmissionAxis2(dJointID, dVector3 result);

void dJointSetTransmissionAnchor1(dJointID, dReal x, dReal y, dReal z);
void dJointSetTransmissionAnchor2(dJointID, dReal x, dReal y, dReal z);
void dJointGetTransmissionAnchor1(dJointID, dVector3 result);
void dJointGetTransmissionAnchor2(dJointID, dVector3 result);

void dJointSetTransmissionRatio( dJointID j, dReal ratio );

dReal dJointGetTransmissionRatio( dJointID j );

void dJointSetTransmissionAxis( dJointID j, dReal x, dReal y, dReal z );
void dJointGetTransmissionAxis( dJointID j, dVector3 result );

dReal dJointGetTransmissionAngle1( dJointID j );
dReal dJointGetTransmissionAngle2( dJointID j );

dReal dJointGetTransmissionRadius1( dJointID j );
dReal dJointGetTransmissionRadius2( dJointID j );

void dJointSetTransmissionRadius1( dJointID j, dReal radius );
void dJointSetTransmissionRadius2( dJointID j, dReal radius );

void dJointSetTransmissionBacklash( dJointID j, dReal backlash );
dReal dJointGetTransmissionBacklash( dJointID j );

void dJointSetHingeParam (dJointID, int parameter, dReal value);
void dJointSetSliderParam (dJointID, int parameter, dReal value);
void dJointSetHinge2Param (dJointID, int parameter, dReal value);
void dJointSetUniversalParam (dJointID, int parameter, dReal value);
void dJointSetAMotorParam (dJointID, int parameter, dReal value);
void dJointSetLMotorParam (dJointID, int parameter, dReal value);
void dJointSetPRParam (dJointID, int parameter, dReal value);
void dJointSetPUParam (dJointID, int parameter, dReal value);
void dJointSetPistonParam (dJointID, int parameter, dReal value);
void dJointSetDBallParam(dJointID, int parameter, dReal value);
void dJointSetDHingeParam(dJointID, int parameter, dReal value);
void dJointSetTransmissionParam(dJointID, int parameter, dReal value); 
dReal dJointGetHingeParam (dJointID, int parameter);
dReal dJointGetSliderParam (dJointID, int parameter);
dReal dJointGetHinge2Param (dJointID, int parameter);
dReal dJointGetUniversalParam (dJointID, int parameter);
dReal dJointGetAMotorParam (dJointID, int parameter);
dReal dJointGetLMotorParam (dJointID, int parameter);
dReal dJointGetPRParam (dJointID, int parameter);
dReal dJointGetPUParam (dJointID, int parameter);
dReal dJointGetPistonParam (dJointID, int parameter);
dReal dJointGetDBallParam(dJointID, int parameter);
dReal dJointGetDHingeParam(dJointID, int parameter);
dReal dJointGetTransmissionParam(dJointID, int parameter);

void dJointAddHingeTorque (dJointID, dReal torque);

void dJointAddUniversalTorques (dJointID, dReal torque1, dReal torque2);

void dJointAddSliderForce (dJointID, dReal force);

void dJointAddHinge2Torques (dJointID, dReal torque1, dReal torque2);

void dJointAddAMotorTorques (dJointID, dReal torque0, dReal torque1, dReal torque2);


-}
