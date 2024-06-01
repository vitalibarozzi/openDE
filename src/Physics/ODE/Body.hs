module Physics.ODE.Body (
    position,
    quaternion,
    rotation,
    linearVel,
    angularVel,
    mass,
    force,
    torque,
    gravityMode,
    finiteRotationMode,
    finiteRotationAxis,
    bodyEnable,
    bodyData,
    rawBodyData,
    safeBodyData,
    -- %TODO
    addForce,
    addTorque,
    create,
    destroyBody,
    isBodyEnabled,
    tryGetSafeBodyData,
    getNumJoints,
    getJoint,
)
where

import Data.Maybe
import Data.StateVar
import Data.Typeable
import Foreign
import Physics.ODE.Raw.Body
import qualified Physics.ODE.Raw.Mass as Mass (create)
import Physics.ODE.Raw.Types
import Physics.ODE.Raw.Utilities

-----------------------------------------------------------
position :: Body -> StateVar (ODEreal, ODEreal, ODEreal)
position body =
    StateVar
        (peekVector3 =<< getBodyPositiondBodyGetPosition body)
        (\(x, y, z) -> setBodyPositiondBodySetPosition body x y z)

-----------------------------------------------------------
quaternion :: Body -> StateVar (ODEreal, ODEreal, ODEreal, ODEreal)
quaternion body =
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr

-----------------------------------------------------------
mass body = do
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr
setMass :: Body -> Mass -> IO ()
setMass = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            withForeignPtr
                arg_1
                ( \marshaledArg_4 -> do
                    ret_5 <- setMassdBodySetMass marshaledArg_3 marshaledArg_4
                    case () of
                        () -> do return ()
                )
        )
getMass :: Body -> IO Mass
getMass body =
    Mass.create
        >>= \mass ->
            withForeignPtr mass $ \cMass -> cGetMass body cMass >> return mass

-----------------------------------------------------------
rotation body = do
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr
setBodyRotation :: Body -> Matrix3 -> IO ()
setBodyRotation = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- setBodyRotationdBodySetRotation marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )
getBodyRotation :: Body -> IO Matrix3
getBodyRotation = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getBodyRotationdBodyGetRotation marshaledArg_2
            return (ret_3)
        )

-----------------------------------------------------------
force body = do
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr
setForce :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
setForce = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- setForcedBodySetForce marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> do return ()
                                )
                        )
                )
        )
getForce :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getForce = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getForcedBodyGetForce marshaledArg_2
            peekVector3 (ret_3)
        )

-----------------------------------------------------------
torque body = do
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr
setTorque :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
setTorque = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- setTorquedBodySetTorque marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> do return ()
                                )
                        )
                )
        )
getTorque :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getTorque = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getTorquedBodyGetTorque marshaledArg_2
            peekVector3 (ret_3)
        )

-----------------------------------------------------------
gravityMode body = do
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr
setGravityMode :: Body -> Bool -> IO ()
setGravityMode = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 (fromBool arg_1))
                ( \marshaledArg_5 -> do
                    ret_6 <- setGravityModedBodySetGravityMode marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )
getGravityMode :: Body -> IO Bool
getGravityMode = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getGravityModedBodyGetGravityMode marshaledArg_2
            return (toBool (ret_3))
        )

-----------------------------------------------------------
bodyData :: Body -> StateVar a
bodyData body = do
    StateVar get_ set
  where
    get_ = getRawBodyData body >>= deRefStablePtr . castPtrToStablePtr
    set value = newStablePtr value >>= \stablePtr -> setRawBodyData body (castStablePtrToPtr stablePtr)

-----------------------------------------------------------
linearVel body = do
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr
setLinearVel :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
setLinearVel = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- setLinearVeldBodySetLinearVel marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> do return ()
                                )
                        )
                )
        )
getLinearVel :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getLinearVel = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getLinearVeldBodyGetLinearVel marshaledArg_2
            peekVector3 (ret_3)
        )

-----------------------------------------------------------
angularVel body = do
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr
setAngularVel :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
setAngularVel = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- setAngularVeldBodySetAngularVel marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> do return ()
                                )
                        )
                )
        )
getAngularVel :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getAngularVel = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getAngularVeldBodyGetAngularVel marshaledArg_2
            peekVector3 (ret_3)
        )

-----------------------------------------------------------
bodyEnable body = do
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr
enableBody :: Body -> IO ()
enableBody = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- enableBodydBodyEnable marshaledArg_2
            case () of
                () -> do return ()
        )
disableBody :: Body -> IO ()
disableBody = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- disableBodydBodyDisable marshaledArg_2
            case () of
                () -> do return ()
        )

-----------------------------------------------------------
rawBodyData body = do
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr

-----------------------------------------------------------
finiteRotationMode :: Body -> StateVar RotationMode
finiteRotationMode body = do
    StateVar get_ set
  where
    get_ = do
        n <- getFiniteRotationMode_ body
        if n == 0
            then return Infinitesimal
            else do
                (x, y, z) <- getFiniteRotationAxis_ body
                return $ Finite x y z
    set value =
        case value of
            Infinitesimal -> setFiniteRotationMode_ body 0
            Finite x y z -> do
                setFiniteRotationMode_ body 1
                setFiniteRotationAxis_ body x y z
setFiniteRotationMode_ :: Body -> Int -> IO ()
setFiniteRotationMode_ = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- setFiniteRotationMode_dBodySetFiniteRotationMode marshaledArg_3 marshaledArg_5
                    case () of
                        () -> do return ()
                )
        )
getFiniteRotationMode_ :: Body -> IO Int
getFiniteRotationMode_ = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- getFiniteRotationMode_dBodyGetFiniteRotationMode marshaledArg_2
            return (ret_3)
        )

-----------------------------------------------------------
finiteRotationAxis =
    StateVar get_ set
  where
    get_ = undefined
    set value = undefined
setFiniteRotationAxis_ ::
    Body ->
    ODEreal ->
    ODEreal ->
    ODEreal ->
    IO ()
setFiniteRotationAxis_ = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- dBodySetFiniteRotationAxis marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> do return ()
                                )
                        )
                )
        )
getFiniteRotationAxis_ :: Body -> IO ((ODEreal, ODEreal, ODEreal))
getFiniteRotationAxis_ = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 ->
            allocaArray
                4
                ( \marshaledArg_3 -> do
                    ret_4 <- dBodyGetFiniteRotationAxis marshaledArg_2 marshaledArg_3
                    peekVector3 (marshaledArg_3)
                )
        )

-----------------------------------------------------------
safeBodyData body = do
    StateVar get_ set
  where
    get_ = fmap (fromMaybe (error errMsg)) . tryGetSafeBodyData $ body
    errMsg = "Physics.ODE.Body.getSafeBodyData: invalid type."
    set value = undefined -- TODO setBodyData body (typeOf value, value)
    -----------------------------------------------------------

tryGetSafeBodyData :: (Typeable a) => Body -> IO (Maybe a)
tryGetSafeBodyData body =
    -- TODO
    undefined -- getBodyData body
    -- >>= \(t, d) ->
    --     if t == typeOf d then return (Just d) else return Nothing
    -----------------------------------------------------------

create :: World -> IO Body
create = createdBodyCreate

-----------------------------------------------------------
destroyBody :: Body -> IO ()
destroyBody = destroyBodydBodyDestroy

-----------------------------------------------------------
addForce :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
addForce = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- addForcedBodyAddForce marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> do return ()
                                )
                        )
                )
        )

-----------------------------------------------------------
addTorque :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
addTorque = \arg_0 arg_1 arg_2 arg_3 ->
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- addTorquedBodyAddTorque marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> do return ()
                                )
                        )
                )
        )

-----------------------------------------------------------
isBodyEnabled :: Body -> IO Bool
isBodyEnabled = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- isBodyEnableddBodyIsEnabled marshaledArg_2
            return (toBool (ret_3))
        )

-----------------------------------------------------------
getNumJoints :: Body -> IO Int
getNumJoints = \arg_0 ->
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- dBodyGetNumJoints marshaledArg_2
            return (ret_3)
        )

-----------------------------------------------------------
getJoint :: Body -> Int -> IO Joint
getJoint = \arg_0 arg_1 ->
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- getJointdBodyGetJoint marshaledArg_3 marshaledArg_5
                    return (ret_6)
                )
        )
