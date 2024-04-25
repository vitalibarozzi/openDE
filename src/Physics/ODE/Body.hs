module Physics.ODE.Body
       (create, destroyBody, setBodyPosition, getBodyPosition,
        setBodyQuaternion, getBodyQuaternion, setBodyRotation,
        getBodyRotation, setLinearVel, getLinearVel, setAngularVel,
        getAngularVel, setMass, getMass, addForce, setForce, getForce,
        addTorque, setTorque, getTorque, enableBody, disableBody,
        isBodyEnabled, setRawBodyData, setBodyData, setSafeBodyData,
        getRawBodyData, getBodyData, tryGetSafeBodyData, getSafeBodyData,
        setFiniteRotationMode, getFiniteRotationMode, getNumJoints,
        getJoint, setGravityMode, getGravityMode)
       where
import Foreign
import Data.Typeable
import Data.Maybe
import qualified Physics.ODE.Mass as Mass (create)
import Physics.ODE.Types
import Physics.ODE.Utilities
 
foreign import ccall unsafe "dBodyGetMass" cGetMass ::
               Ptr BodyStruct -> Ptr MassStruct -> IO ()
 
getMass :: Body -> IO Mass
getMass body
  = Mass.create >>=
      \ mass ->
        withForeignPtr mass $ \ cMass -> cGetMass body cMass >> return mass
 
foreign import ccall unsafe "dBodySetData" setRawBodyData ::
               Ptr BodyStruct -> Ptr a -> IO ()
 
setBodyData :: Body -> a -> IO ()
setBodyData body d
  = newStablePtr d >>=
      \ stablePtr -> setRawBodyData body (castStablePtrToPtr stablePtr)
 
setSafeBodyData :: (Typeable a) => Body -> a -> IO ()
setSafeBodyData body d = setBodyData body (typeOf d, d)
 
foreign import ccall unsafe "dBodyGetData" getRawBodyData ::
               Ptr BodyStruct -> IO (Ptr a)
 
getBodyData :: Body -> IO a
getBodyData body
  = getRawBodyData body >>= deRefStablePtr . castPtrToStablePtr
 
tryGetSafeBodyData :: (Typeable a) => Body -> IO (Maybe a)
tryGetSafeBodyData body
  = getBodyData body >>=
      \ (t, d) ->
        if t == typeOf d then return (Just d) else return Nothing
 
getSafeBodyData :: (Typeable a) => Body -> IO a
getSafeBodyData
  = fmap (fromMaybe (error errMsg)) . tryGetSafeBodyData
  where errMsg = "Physics.ODE.Body.getSafeBodyData: invalid type."
 
setFiniteRotationMode :: Body -> RotationMode -> IO ()
setFiniteRotationMode body (Infinitesimal)
  = setFiniteRotationMode_ body 0
setFiniteRotationMode body (Finite x y z)
  = do setFiniteRotationMode_ body 1
       setFiniteRotationAxis_ body x y z
 
getFiniteRotationMode :: Body -> IO RotationMode
getFiniteRotationMode body
  = do n <- getFiniteRotationMode_ body
       if n == 0 then return Infinitesimal else
         do (x, y, z) <- getFiniteRotationAxis_ body
            return $ Finite x y z
create :: World -> IO Body
create = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- createdBodyCreate marshaledArg_2
                                                                        return (ret_3))
foreign import ccall unsafe "dBodyCreate" createdBodyCreate :: World ->
                                                               IO Body
destroyBody :: Body -> IO ()
destroyBody = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- destroyBodydBodyDestroy marshaledArg_2
                                                                             case () of
                                                                                 () -> do return ())
foreign import ccall unsafe "dBodyDestroy" destroyBodydBodyDestroy :: Body ->
                                                                      IO ()
setBodyPosition :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
setBodyPosition = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setBodyPositiondBodySetPosition marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                            case () of
                                                                                                                                                                                                                                                                () -> do return ()))))
foreign import ccall unsafe "dBodySetPosition" setBodyPositiondBodySetPosition :: Body ->
                                                                                  ODEreal ->
                                                                                  ODEreal ->
                                                                                  ODEreal -> IO ()
getBodyPosition :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getBodyPosition = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getBodyPositiondBodyGetPosition marshaledArg_2
                                                                                 peekVector3 (ret_3))
foreign import ccall unsafe "dBodyGetPosition" getBodyPositiondBodyGetPosition :: Body ->
                                                                                  IO (Ptr ODEreal)
setBodyQuaternion :: Body ->
                     (ODEreal, ODEreal, ODEreal, ODEreal) -> IO ()
setBodyQuaternion = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> allocaArray 4 (\ptr_5 -> (>>) (pokeArray ptr_5 (case arg_1 of
                                                                                                                                                        (a_6,
                                                                                                                                                         b_7,
                                                                                                                                                         c_8,
                                                                                                                                                         d_9) -> [a_6,
                                                                                                                                                                  b_7,
                                                                                                                                                                  c_8,
                                                                                                                                                                  d_9])) (action_4 ptr_5))) (\marshaledArg_10 -> do ret_11 <- setBodyQuaterniondBodySetQuaternion marshaledArg_3 marshaledArg_10
                                                                                                                                                                                                                    case () of
                                                                                                                                                                                                                        () -> do return ()))
foreign import ccall unsafe "dBodySetQuaternion" setBodyQuaterniondBodySetQuaternion :: Body ->
                                                                                        Ptr ODEreal ->
                                                                                        IO ()
getBodyQuaternion :: Body ->
                     IO ((ODEreal, ODEreal, ODEreal, ODEreal))
getBodyQuaternion = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getBodyQuaterniondBodyGetQuaternion marshaledArg_2
                                                                                   peekVector4 (ret_3))
foreign import ccall unsafe "dBodyGetQuaternion" getBodyQuaterniondBodyGetQuaternion :: Body ->
                                                                                        IO (Ptr ODEreal)
setBodyRotation :: Body -> Matrix3 -> IO ()
setBodyRotation = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 arg_1) (\marshaledArg_5 -> do ret_6 <- setBodyRotationdBodySetRotation marshaledArg_3 marshaledArg_5
                                                                                                                                         case () of
                                                                                                                                             () -> do return ()))
foreign import ccall unsafe "dBodySetRotation" setBodyRotationdBodySetRotation :: Body ->
                                                                                  Matrix3 -> IO ()
getBodyRotation :: Body -> IO Matrix3
getBodyRotation = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getBodyRotationdBodyGetRotation marshaledArg_2
                                                                                 return (ret_3))
foreign import ccall unsafe "dBodyGetRotation" getBodyRotationdBodyGetRotation :: Body ->
                                                                                  IO Matrix3
setLinearVel :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
setLinearVel = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setLinearVeldBodySetLinearVel marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                         case () of
                                                                                                                                                                                                                                                             () -> do return ()))))
foreign import ccall unsafe "dBodySetLinearVel" setLinearVeldBodySetLinearVel :: Body ->
                                                                                 ODEreal ->
                                                                                 ODEreal ->
                                                                                 ODEreal -> IO ()
getLinearVel :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getLinearVel = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getLinearVeldBodyGetLinearVel marshaledArg_2
                                                                              peekVector3 (ret_3))
foreign import ccall unsafe "dBodyGetLinearVel" getLinearVeldBodyGetLinearVel :: Body ->
                                                                                 IO (Ptr ODEreal)
setAngularVel :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
setAngularVel = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setAngularVeldBodySetAngularVel marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                          case () of
                                                                                                                                                                                                                                                              () -> do return ()))))
foreign import ccall unsafe "dBodySetAngularVel" setAngularVeldBodySetAngularVel :: Body ->
                                                                                    ODEreal ->
                                                                                    ODEreal ->
                                                                                    ODEreal -> IO ()
getAngularVel :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getAngularVel = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getAngularVeldBodyGetAngularVel marshaledArg_2
                                                                               peekVector3 (ret_3))
foreign import ccall unsafe "dBodyGetAngularVel" getAngularVeldBodyGetAngularVel :: Body ->
                                                                                    IO (Ptr ODEreal)
setMass :: Body -> Mass -> IO ()
setMass = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> withForeignPtr arg_1 (\marshaledArg_4 -> do ret_5 <- setMassdBodySetMass marshaledArg_3 marshaledArg_4
                                                                                                                        case () of
                                                                                                                            () -> do return ()))
foreign import ccall unsafe "dBodySetMass" setMassdBodySetMass :: Body ->
                                                                  Ptr MassStruct -> IO ()
addForce :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
addForce = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- addForcedBodyAddForce marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                     case () of
                                                                                                                                                                                                                                                         () -> do return ()))))
foreign import ccall unsafe "dBodyAddForce" addForcedBodyAddForce :: Body ->
                                                                     ODEreal ->
                                                                     ODEreal -> ODEreal -> IO ()
setForce :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
setForce = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setForcedBodySetForce marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                     case () of
                                                                                                                                                                                                                                                         () -> do return ()))))
foreign import ccall unsafe "dBodySetForce" setForcedBodySetForce :: Body ->
                                                                     ODEreal ->
                                                                     ODEreal -> ODEreal -> IO ()
getForce :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getForce = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getForcedBodyGetForce marshaledArg_2
                                                                          peekVector3 (ret_3))
foreign import ccall unsafe "dBodyGetForce" getForcedBodyGetForce :: Body ->
                                                                     IO (Ptr ODEreal)
addTorque :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
addTorque = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- addTorquedBodyAddTorque marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                      case () of
                                                                                                                                                                                                                                                          () -> do return ()))))
foreign import ccall unsafe "dBodyAddTorque" addTorquedBodyAddTorque :: Body ->
                                                                        ODEreal ->
                                                                        ODEreal -> ODEreal -> IO ()
setTorque :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
setTorque = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setTorquedBodySetTorque marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                      case () of
                                                                                                                                                                                                                                                          () -> do return ()))))
foreign import ccall unsafe "dBodySetTorque" setTorquedBodySetTorque :: Body ->
                                                                        ODEreal ->
                                                                        ODEreal -> ODEreal -> IO ()
getTorque :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getTorque = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getTorquedBodyGetTorque marshaledArg_2
                                                                           peekVector3 (ret_3))
foreign import ccall unsafe "dBodyGetTorque" getTorquedBodyGetTorque :: Body ->
                                                                        IO (Ptr ODEreal)
enableBody :: Body -> IO ()
enableBody = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- enableBodydBodyEnable marshaledArg_2
                                                                            case () of
                                                                                () -> do return ())
foreign import ccall unsafe "dBodyEnable" enableBodydBodyEnable :: Body ->
                                                                   IO ()
disableBody :: Body -> IO ()
disableBody = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- disableBodydBodyDisable marshaledArg_2
                                                                             case () of
                                                                                 () -> do return ())
foreign import ccall unsafe "dBodyDisable" disableBodydBodyDisable :: Body ->
                                                                      IO ()
isBodyEnabled :: Body -> IO Bool
isBodyEnabled = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- isBodyEnableddBodyIsEnabled marshaledArg_2
                                                                               return (toBool (ret_3)))
foreign import ccall unsafe "dBodyIsEnabled" isBodyEnableddBodyIsEnabled :: Body ->
                                                                            IO Int
setFiniteRotationMode_ :: Body -> Int -> IO ()
setFiniteRotationMode_ = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 arg_1) (\marshaledArg_5 -> do ret_6 <- setFiniteRotationMode_dBodySetFiniteRotationMode marshaledArg_3 marshaledArg_5
                                                                                                                                                case () of
                                                                                                                                                    () -> do return ()))
foreign import ccall unsafe "dBodySetFiniteRotationMode" setFiniteRotationMode_dBodySetFiniteRotationMode :: Body ->
                                                                                                             Int ->
                                                                                                             IO ()
getFiniteRotationMode_ :: Body -> IO Int
getFiniteRotationMode_ = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getFiniteRotationMode_dBodyGetFiniteRotationMode marshaledArg_2
                                                                                        return (ret_3))
foreign import ccall unsafe "dBodyGetFiniteRotationMode" getFiniteRotationMode_dBodyGetFiniteRotationMode :: Body ->
                                                                                                             IO Int
setFiniteRotationAxis_ :: Body ->
                          ODEreal -> ODEreal -> ODEreal -> IO ()
setFiniteRotationAxis_ = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setFiniteRotationAxis_dBodySetFiniteRotationAxis marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                                   case () of
                                                                                                                                                                                                                                                                       () -> do return ()))))
foreign import ccall unsafe "dBodySetFiniteRotationAxis" setFiniteRotationAxis_dBodySetFiniteRotationAxis :: Body ->
                                                                                                             ODEreal ->
                                                                                                             ODEreal ->
                                                                                                             ODEreal ->
                                                                                                             IO ()
getFiniteRotationAxis_ :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getFiniteRotationAxis_ = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> allocaArray 4 (\marshaledArg_3 -> do ret_4 <- getFiniteRotationAxis_dBodyGetFiniteRotationAxis marshaledArg_2 marshaledArg_3
                                                                                                                          peekVector3 (marshaledArg_3)))
foreign import ccall unsafe "dBodyGetFiniteRotationAxis" getFiniteRotationAxis_dBodyGetFiniteRotationAxis :: Body ->
                                                                                                             Ptr ODEreal ->
                                                                                                             IO ()
getNumJoints :: Body -> IO Int
getNumJoints = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getNumJointsdBodyGetNumJoints marshaledArg_2
                                                                              return (ret_3))
foreign import ccall unsafe "dBodyGetNumJoints" getNumJointsdBodyGetNumJoints :: Body ->
                                                                                 IO Int
getJoint :: Body -> Int -> IO Joint
getJoint = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 arg_1) (\marshaledArg_5 -> do ret_6 <- getJointdBodyGetJoint marshaledArg_3 marshaledArg_5
                                                                                                                                  return (ret_6)))
foreign import ccall unsafe "dBodyGetJoint" getJointdBodyGetJoint :: Body ->
                                                                     Int -> IO Joint
setGravityMode :: Body -> Bool -> IO ()
setGravityMode = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 (fromBool arg_1)) (\marshaledArg_5 -> do ret_6 <- setGravityModedBodySetGravityMode marshaledArg_3 marshaledArg_5
                                                                                                                                                   case () of
                                                                                                                                                       () -> do return ()))
foreign import ccall unsafe "dBodySetGravityMode" setGravityModedBodySetGravityMode :: Body ->
                                                                                       Int -> IO ()
getGravityMode :: Body -> IO Bool
getGravityMode = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getGravityModedBodyGetGravityMode marshaledArg_2
                                                                                return (toBool (ret_3)))
foreign import ccall unsafe "dBodyGetGravityMode" getGravityModedBodyGetGravityMode :: Body ->
                                                                                       IO Int
