module Physics.ODE.Joint (
    createBall,
    createHinge,
    createSlider,
    createContact,
    createGroup,
    destroyJoint,
    destroyGroup,
    emptyGroup,
    attach,
    setJointData,
    setSafeJointData,
    getJointData,
    getSafeJointData,
    tryGetSafeJointData,
    getType,
    getBody,
    areConnected,
    areConnectedExcluding,
    setBallAnchor,
    getBallAnchor,
    setHingeAnchor,
    setHingeAxis,
)
where

import Data.Maybe
import Data.Typeable
import Foreign
import Physics.ODE.Raw.Hsc
import Physics.ODE.Raw.Joint
import Physics.ODE.Raw.Types
import Physics.ODE.Utilities

createGroup :: IO JointGroup
createGroup = createGroup' 0

setJointData :: Joint -> a -> IO ()
setJointData body d =
    newStablePtr d
        >>= \stablePtr -> c'setRawJointData body (castStablePtrToPtr stablePtr)

setSafeJointData :: (Typeable a) => Joint -> a -> IO ()
setSafeJointData body d = setJointData body (typeOf d, d)

getJointData :: Joint -> IO a
getJointData body =
    c'getRawJointData body >>= deRefStablePtr . castPtrToStablePtr

tryGetSafeJointData :: (Typeable a) => Joint -> IO (Maybe a)
tryGetSafeJointData body =
    getJointData body
        >>= \(t, d) ->
            if t == typeOf d then return (Just d) else return Nothing

getSafeJointData :: (Typeable a) => Joint -> IO a
getSafeJointData =
    fmap (fromMaybe (error errMsg)) . tryGetSafeJointData
  where
    errMsg = "Physics.ODE.Joint.getSafeJointData: invalid type."

--  import qualified Physics.ODE.Mass as Mass ( create )
createBall :: World -> Maybe JointGroup -> IO Joint
createBall arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            ( case arg_1 of
                Data.Maybe.Just a_4 -> \action_5 -> action_5 a_4
                Data.Maybe.Nothing -> \action_6 -> action_6 nullPtr
            )
                ( \marshaledArg_7 -> do
                    ret_8 <- c'createBalldJointCreateBall marshaledArg_3 marshaledArg_7
                    return ret_8
                )
        )
createHinge :: World -> Maybe JointGroup -> IO Joint
createHinge arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            ( case arg_1 of
                Data.Maybe.Just a_4 -> \action_5 -> action_5 a_4
                Data.Maybe.Nothing -> \action_6 -> action_6 nullPtr
            )
                ( \marshaledArg_7 -> do
                    ret_8 <- c'createHingedJointCreateHinge marshaledArg_3 marshaledArg_7
                    return ret_8
                )
        )
createSlider :: World -> Maybe JointGroup -> IO Joint
createSlider arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            ( case arg_1 of
                Data.Maybe.Just a_4 -> \action_5 -> action_5 a_4
                Data.Maybe.Nothing -> \action_6 -> action_6 nullPtr
            )
                ( \marshaledArg_7 -> do
                    ret_8 <- c'createSliderdJointCreateSlider marshaledArg_3 marshaledArg_7
                    return ret_8
                )
        )
createContact ::
    World ->
    Maybe JointGroup ->
    ContactInfo ->
    IO Joint
createContact arg_0 arg_1 arg_2 =
    (\action_3 -> action_3 arg_0)
        ( \marshaledArg_4 ->
            ( case arg_1 of
                Data.Maybe.Just a_5 -> \action_6 -> action_6 a_5
                Data.Maybe.Nothing -> \action_7 -> action_7 nullPtr
            )
                ( \marshaledArg_8 ->
                    ( \action_9 ->
                        alloca
                            ( \ptr_10 -> do
                                poke ptr_10 arg_2
                                action_9 ptr_10
                            )
                    )
                        ( \marshaledArg_11 -> do
                            ret_12 <- c'createContactdJointCreateContact marshaledArg_4 marshaledArg_8 marshaledArg_11
                            return ret_12
                        )
                )
        )
createGroup' :: Int -> IO JointGroup
createGroup' arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- c'createGroup'dJointGroupCreate marshaledArg_2
            return ret_3
        )
destroyJoint :: Joint -> IO ()
destroyJoint arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- c'destroyJointdJointDestroy marshaledArg_2
            case () of
                () -> return ()
        )
destroyGroup :: JointGroup -> IO ()
destroyGroup arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- c'destroyGroupdJointGroupDestroy marshaledArg_2
            case () of
                () -> return ()
        )
emptyGroup :: JointGroup -> IO ()
emptyGroup arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- c'emptyGroupdJointGroupEmpty marshaledArg_2
            case () of
                () -> return ()
        )
attach :: Joint -> Maybe Body -> Maybe Body -> IO ()
attach arg_0 arg_1 arg_2 =
    (\action_3 -> action_3 arg_0)
        ( \marshaledArg_4 ->
            ( case arg_1 of
                Data.Maybe.Just a_5 -> \action_6 -> action_6 a_5
                Data.Maybe.Nothing -> \action_7 -> action_7 nullPtr
            )
                ( \marshaledArg_8 ->
                    ( case arg_2 of
                        Data.Maybe.Just a_9 -> \action_10 -> action_10 a_9
                        Data.Maybe.Nothing -> \action_11 -> action_11 nullPtr
                    )
                        ( \marshaledArg_12 -> do
                            ret_13 <- c'attachdJointAttach marshaledArg_4 marshaledArg_8 marshaledArg_12
                            case () of
                                () -> return ()
                        )
                )
        )
getType :: Joint -> IO JointType
getType arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 -> do
            ret_3 <- c'getTypedJointGetType marshaledArg_2
            return (toJointType ret_3)
        )
getBody :: Joint -> BodyIndex -> IO Body
getBody arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 (fromBodyIndex arg_1))
                ( \marshaledArg_5 -> do
                    ret_6 <- c'getBodydJointGetBody marshaledArg_3 marshaledArg_5
                    return ret_6
                )
        )
areConnected :: Body -> Body -> IO Bool
areConnected arg_0 arg_1 =
    (\action_2 -> action_2 arg_0)
        ( \marshaledArg_3 ->
            (\action_4 -> action_4 arg_1)
                ( \marshaledArg_5 -> do
                    ret_6 <- c'areConnecteddAreConnected marshaledArg_3 marshaledArg_5
                    return (toBool ret_6)
                )
        )
areConnectedExcluding :: Body -> Body -> JointType -> IO Bool
areConnectedExcluding arg_0 arg_1 arg_2 =
    (\action_3 -> action_3 arg_0)
        ( \marshaledArg_4 ->
            (\action_5 -> action_5 arg_1)
                ( \marshaledArg_6 ->
                    (\action_7 -> action_7 (fromJointType arg_2))
                        ( \marshaledArg_8 -> do
                            ret_9 <- c'areConnectedExcludingdAreConnectedExcluding marshaledArg_4 marshaledArg_6 marshaledArg_8
                            return (toBool ret_9)
                        )
                )
        )
setBallAnchor :: Joint -> Float -> Float -> Float -> IO ()
setBallAnchor arg_0 arg_1 arg_2 arg_3 =
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- c'setBallAnchordJointSetBallAnchor marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> return ()
                                )
                        )
                )
        )
getBallAnchor :: Joint -> IO (Float, Float, Float)
getBallAnchor arg_0 =
    (\action_1 -> action_1 arg_0)
        ( \marshaledArg_2 ->
            allocaArray
                4
                ( \marshaledArg_3 -> do
                    ret_4 <- c'getBallAnchordJointGetBallAnchor marshaledArg_2 marshaledArg_3
                    peekVector3 marshaledArg_3
                )
        )

-- -------------------------------------------
--  Hinge joint
setHingeAnchor :: Joint -> Float -> Float -> Float -> IO ()
setHingeAnchor arg_0 arg_1 arg_2 arg_3 =
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- c'setHingeAnchordJointSetHingeAnchor marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> return ()
                                )
                        )
                )
        )
setHingeAxis :: Joint -> Float -> Float -> Float -> IO ()
setHingeAxis arg_0 arg_1 arg_2 arg_3 =
    (\action_4 -> action_4 arg_0)
        ( \marshaledArg_5 ->
            (\action_6 -> action_6 arg_1)
                ( \marshaledArg_7 ->
                    (\action_8 -> action_8 arg_2)
                        ( \marshaledArg_9 ->
                            (\action_10 -> action_10 arg_3)
                                ( \marshaledArg_11 -> do
                                    ret_12 <- c'setHingeAxisdJointSetHingeAxis marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                    case () of
                                        () -> return ()
                                )
                        )
                )
        )
