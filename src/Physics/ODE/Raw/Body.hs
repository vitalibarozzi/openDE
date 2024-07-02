module Physics.ODE.Raw.Body where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dBodyGetJoint" getJointdBodyGetJoint :: Body -> Int -> IO Joint
foreign import ccall unsafe "dBodySetGravityMode" setGravityModedBodySetGravityMode :: Body -> Int -> IO ()
foreign import ccall unsafe "dBodyGetMass" cGetMass :: Ptr BodyStruct -> Ptr MassStruct -> IO ()
foreign import ccall unsafe "dBodyGetGravityMode" getGravityModedBodyGetGravityMode :: Body -> IO Int
foreign import ccall unsafe "dBodySetQuaternion" setBodyQuaterniondBodySetQuaternion :: Body -> Ptr Float -> IO ()
foreign import ccall unsafe "dBodyCreate" createdBodyCreate :: World -> IO Body
foreign import ccall unsafe "dBodySetData" setRawBodyData :: Ptr BodyStruct -> Ptr a -> IO ()
foreign import ccall unsafe "dBodyGetData" getRawBodyData :: Ptr BodyStruct -> IO (Ptr a)
foreign import ccall unsafe "dBodyDestroy" destroyBodydBodyDestroy :: Body -> IO ()
foreign import ccall unsafe "dBodySetPosition" setBodyPositiondBodySetPosition :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetPosition" getBodyPositiondBodyGetPosition :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodyGetQuaternion" getBodyQuaterniondBodyGetQuaternion :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodySetRotation" setBodyRotationdBodySetRotation :: Body -> Matrix3 -> IO ()
foreign import ccall unsafe "dBodyGetRotation" getBodyRotationdBodyGetRotation :: Body -> IO Matrix3
foreign import ccall unsafe "dBodySetLinearVel" setLinearVeldBodySetLinearVel :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetLinearVel" getLinearVeldBodyGetLinearVel :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodySetAngularVel" setAngularVeldBodySetAngularVel :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetAngularVel" getAngularVeldBodyGetAngularVel :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodySetMass" setMassdBodySetMass :: Body -> Ptr MassStruct -> IO ()
foreign import ccall unsafe "dBodyAddForce" addForcedBodyAddForce :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodySetForce" setForcedBodySetForce :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetForce" getForcedBodyGetForce :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodyAddTorque" addTorquedBodyAddTorque :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodySetTorque" setTorquedBodySetTorque :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetTorque" getTorquedBodyGetTorque :: Body -> IO (Ptr Float)
foreign import ccall unsafe "dBodyEnable" enableBodydBodyEnable :: Body -> IO ()
foreign import ccall unsafe "dBodyDisable" disableBodydBodyDisable :: Body -> IO ()
foreign import ccall unsafe "dBodyIsEnabled" isBodyEnableddBodyIsEnabled :: Body -> IO Int
foreign import ccall unsafe "dBodySetFiniteRotationMode" setFiniteRotationMode_dBodySetFiniteRotationMode :: Body -> Int -> IO ()
foreign import ccall unsafe "dBodyGetFiniteRotationMode" getFiniteRotationMode_dBodyGetFiniteRotationMode :: Body -> IO Int
foreign import ccall unsafe "dBodySetFiniteRotationAxis" dBodySetFiniteRotationAxis :: Body -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dBodyGetFiniteRotationAxis" dBodyGetFiniteRotationAxis :: Body -> Ptr Float -> IO ()
foreign import ccall unsafe "dBodyGetNumJoints" dBodyGetNumJoints :: Body -> IO Int
