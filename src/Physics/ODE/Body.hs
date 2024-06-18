-- | High-level bindings using StateVar where possible.
module Physics.ODE.Body (
    -- * Create / Destroy
    create,
    destroy,

    -- * Body fields
    position,
    quaternion,
    rotation,
    linearVel,
    angularVel,
    mass,
    force,
    torque,
    gravityMode,
    enabled,
    bodyData,
    rawBodyData,

    -- * Utilities
    addForce,
    addTorque,
    isBodyEnabled,
    getNumJoints,
    getJoint,
)
where

import Data.StateVar
import Foreign
import Physics.ODE.Raw.Body
import qualified Physics.ODE.Raw.Mass as Mass (create)
import Physics.ODE.Raw.Types
import Physics.ODE.Raw.Utilities

-----------------------------------------------------------
create :: World -> IO Body
{-# INLINE create #-}
create =
    createdBodyCreate

-----------------------------------------------------------
destroy :: Body -> IO ()
{-# INLINE destroy #-}
destroy =
    destroyBodydBodyDestroy

-----------------------------------------------------------
addForce :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
{-# INLINE addForce #-}
addForce =
    addForcedBodyAddForce

-----------------------------------------------------------
addTorque :: Body -> ODEreal -> ODEreal -> ODEreal -> IO ()
{-# INLINE addTorque #-}
addTorque =
    addTorquedBodyAddTorque

-----------------------------------------------------------
isBodyEnabled :: Body -> IO Bool
{-# INLINE isBodyEnabled #-}
isBodyEnabled body =
    toBool <$> isBodyEnableddBodyIsEnabled body

-----------------------------------------------------------
getNumJoints :: Body -> IO Int
{-# INLINE getNumJoints #-}
getNumJoints =
    dBodyGetNumJoints

-----------------------------------------------------------
getJoint :: Body -> Int -> IO Joint
{-# INLINE getJoint #-}
getJoint =
    getJointdBodyGetJoint

-----------------------------------------------------------
position :: Body -> StateVar (ODEreal, ODEreal, ODEreal)
{-# INLINE position #-}
position body =
    StateVar
        (peekVector3 =<< getBodyPositiondBodyGetPosition body)
        (\(x, y, z) -> setBodyPositiondBodySetPosition body x y z)

-----------------------------------------------------------
quaternion :: Body -> StateVar (ODEreal, ODEreal, ODEreal, ODEreal)
{-# INLINE quaternion #-}
quaternion body =
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr

-----------------------------------------------------------
mass :: Body -> StateVar Mass
{-# INLINE mass #-}
mass body =
    StateVar get_ set
  where
    get_ = Mass.create >>= \mass_ -> withForeignPtr mass_ $ \cMass -> cGetMass body cMass >> return mass_
    set mass_ = withForeignPtr mass_ (setMassdBodySetMass body)

-----------------------------------------------------------
rotation :: Body -> StateVar Matrix3
{-# INLINE rotation #-}
rotation body =
    StateVar get_ set
  where
    get_ = getBodyRotationdBodyGetRotation body
    set = setBodyRotationdBodySetRotation body

-----------------------------------------------------------
force :: Body -> StateVar (ODEreal, ODEreal, ODEreal)
{-# INLINE force #-}
force body =
    StateVar get_ set
  where
    get_ = peekVector3 =<< getForcedBodyGetForce body
    set (x, y, z) = setForcedBodySetForce body x y z

-----------------------------------------------------------
torque :: Ptr BodyStruct -> StateVar (ODEreal, ODEreal, ODEreal)
{-# INLINE torque #-}
torque body =
    StateVar get_ set
  where
    get_ = peekVector3 =<< getTorquedBodyGetTorque body
    set = let foo = setTorquedBodySetTorque body in \(x, y, z) -> foo x y z

-----------------------------------------------------------
gravityMode :: Body -> StateVar Bool
{-# INLINE gravityMode #-}
gravityMode body =
    StateVar get_ set_
  where
    get_ = toBool <$> getGravityModedBodyGetGravityMode body
    set_ = setGravityModedBodySetGravityMode body . fromBool

-----------------------------------------------------------
bodyData :: Body -> StateVar a
{-# INLINE bodyData #-}
bodyData body =
    StateVar get_ set
  where
    get_ = getRawBodyData body >>= deRefStablePtr . castPtrToStablePtr
    set value = newStablePtr value >>= \stablePtr -> setRawBodyData body (castStablePtrToPtr stablePtr)

-----------------------------------------------------------
linearVel :: Body -> StateVar (ODEreal, ODEreal, ODEreal)
{-# INLINE linearVel #-}
linearVel body =
    StateVar get_ set_
  where
    get_ = peekVector3 =<< getLinearVeldBodyGetLinearVel body
    set_ (x, y, z) = setLinearVeldBodySetLinearVel body x y z

-----------------------------------------------------------
angularVel :: Body -> StateVar (ODEreal, ODEreal, ODEreal)
{-# INLINE angularVel #-}
angularVel body =
    StateVar get_ set_
  where
    get_ = peekVector3 =<< getAngularVeldBodyGetAngularVel body
    set_ (x, y, z) = setAngularVeldBodySetAngularVel body x y z

-----------------------------------------------------------
enabled :: Body -> StateVar Bool
{-# INLINE enabled #-}
enabled body =
    StateVar get_ set
  where
    get_ = isBodyEnabled body
    set enable = if enable then enableBodydBodyEnable body else disableBodydBodyDisable body

-----------------------------------------------------------
rawBodyData :: Body -> StateVar (ODEreal, ODEreal, ODEreal, ODEreal)
{-# INLINE rawBodyData #-}
rawBodyData body =
    StateVar get_ set
  where
    get_ = peekVector4 =<< getBodyQuaterniondBodyGetQuaternion body
    set value = allocaArray 4 $ \ptr -> do
        pokeArray ptr (case value of (a_6, b_7, c_8, d_9) -> [a_6, b_7, c_8, d_9])
        setBodyQuaterniondBodySetQuaternion body ptr

-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------

{-
-----------------------------------------------------------
tryGetSafeBodyData :: Body -> IO (Maybe a)
tryGetSafeBodyData _body =
    -- TODO
    undefined -- getBodyData body
    -- >>= \(t, d) ->
    --     if t == typeOf d then return (Just d) else return Nothing
    -----------------------------------------------------------

-----------------------------------------------------------
safeBodyData :: Body -> StateVar a
safeBodyData body =
    StateVar get_ set
  where
    get_ = fmap (fromMaybe (error errMsg)) . tryGetSafeBodyData $ body
    errMsg = "Physics.ODE.Body.getSafeBodyData: invalid type."
    set _value = undefined -- TODO setBodyData body (typeOf value, value)
    -----------------------------------------------------------

-----------------------------------------------------------
finiteRotationMode :: Body -> StateVar RotationMode
finiteRotationMode body = StateVar get_ set_
  where
    set_ = undefined
    -- get_ = do
    --    n <- getFiniteRotationMode_ body
    --    if n == 0
    --        then return Infinitesimal
    --        else do
    --            (x, y, z) <- getFiniteRotationAxis_ body
    --            return $ Finite x y z
    -- set value =
    --    case value of
    --        Infinitesimal -> setFiniteRotationMode_ body 0
    --        Finite x y z -> do
    --            setFiniteRotationMode_ body 1
    --            setFiniteRotationAxis_ body x y z
    -- setFiniteRotationMode_ :: Body -> Int -> IO ()
    -- setFiniteRotationMode_ =
    --    \arg_0 arg_1 ->
    --        (\action_2 -> action_2 arg_0)
    --            ( \marshaledArg_3 ->
    --                (\action_4 -> action_4 arg_1)
    --                    ( \marshaledArg_5 -> do
    --                        ret_6 <- setFiniteRotationMode_dBodySetFiniteRotationMode marshaledArg_3 marshaledArg_5
    --                        case () of
    --                            () -> do return ()
    --                    )
    --            )
    get_ :: IO RotationMode
    get_ = undefined $ getFiniteRotationMode_dBodyGetFiniteRotationMode body

-----------------------------------------------------------
finiteRotationAxis :: StateVar a
finiteRotationAxis =
    undefined {-
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
                getFiniteRotationAxis_ :: Body -> IO (ODEreal, ODEreal, ODEreal)
                setFiniteRotationAxis_ arg_0 arg_1 arg_2 arg_3 =
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
                                                        () -> return ()
                                                )
                                        )
                                )
                        )
                getFiniteRotationAxis_ arg_0 =
                    (\action_1 -> action_1 arg_0)
                        ( \marshaledArg_2 ->
                            allocaArray
                                4
                                ( \marshaledArg_3 -> do
                                    ret_4 <- dBodyGetFiniteRotationAxis marshaledArg_2 marshaledArg_3
                                    peekVector3 marshaledArg_3
                                )
                        )
                        -}
                        -}
