module Physics.ODE.Joint
       (createBall, createHinge, createSlider, createContact, createGroup,
        destroyJoint, destroyGroup, emptyGroup, attach, setRawJointData,
        setJointData, setSafeJointData, getRawJointData, getJointData,
        getSafeJointData, tryGetSafeJointData, getType, getBody,
        areConnected, areConnectedExcluding, setBallAnchor, getBallAnchor,
        setHingeAnchor, setHingeAxis)
       where
import Foreign
import Data.Typeable
import Data.Maybe
import Physics.ODE.Types
import Physics.ODE.Utilities
import Physics.ODE.Hsc
 
createGroup :: IO JointGroup
createGroup = createGroup' 0
 
foreign import ccall unsafe "dJointSetData" setRawJointData ::
               Ptr JointStruct -> Ptr a -> IO ()
 
setJointData :: Joint -> a -> IO ()
setJointData body d
  = newStablePtr d >>=
      \ stablePtr -> setRawJointData body (castStablePtrToPtr stablePtr)
 
setSafeJointData :: (Typeable a) => Joint -> a -> IO ()
setSafeJointData body d = setJointData body (typeOf d, d)
 
foreign import ccall unsafe "dJointGetData" getRawJointData ::
               Ptr JointStruct -> IO (Ptr a)
 
getJointData :: Joint -> IO a
getJointData body
  = getRawJointData body >>= deRefStablePtr . castPtrToStablePtr
 
tryGetSafeJointData :: (Typeable a) => Joint -> IO (Maybe a)
tryGetSafeJointData body
  = getJointData body >>=
      \ (t, d) ->
        if t == typeOf d then return (Just d) else return Nothing
 
getSafeJointData :: (Typeable a) => Joint -> IO a
getSafeJointData
  = fmap (fromMaybe (error errMsg)) . tryGetSafeJointData
  where errMsg = "Physics.ODE.Joint.getSafeJointData: invalid type."
--  import qualified Physics.ODE.Mass as Mass ( create )
createBall :: World -> Maybe JointGroup -> IO Joint
createBall = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (case arg_1 of
                                                                                    Data.Maybe.Just a_4 -> \action_5 -> action_5 a_4
                                                                                    Data.Maybe.Nothing -> \action_6 -> action_6 nullPtr) (\marshaledArg_7 -> do ret_8 <- createBalldJointCreateBall marshaledArg_3 marshaledArg_7
                                                                                                                                                                return (ret_8)))
foreign import ccall unsafe "dJointCreateBall" createBalldJointCreateBall :: World ->
                                                                             JointGroup -> IO Joint
createHinge :: World -> Maybe JointGroup -> IO Joint
createHinge = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (case arg_1 of
                                                                                     Data.Maybe.Just a_4 -> \action_5 -> action_5 a_4
                                                                                     Data.Maybe.Nothing -> \action_6 -> action_6 nullPtr) (\marshaledArg_7 -> do ret_8 <- createHingedJointCreateHinge marshaledArg_3 marshaledArg_7
                                                                                                                                                                 return (ret_8)))
foreign import ccall unsafe "dJointCreateHinge" createHingedJointCreateHinge :: World ->
                                                                                JointGroup ->
                                                                                IO Joint
createSlider :: World -> Maybe JointGroup -> IO Joint
createSlider = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (case arg_1 of
                                                                                      Data.Maybe.Just a_4 -> \action_5 -> action_5 a_4
                                                                                      Data.Maybe.Nothing -> \action_6 -> action_6 nullPtr) (\marshaledArg_7 -> do ret_8 <- createSliderdJointCreateSlider marshaledArg_3 marshaledArg_7
                                                                                                                                                                  return (ret_8)))
foreign import ccall unsafe "dJointCreateSlider" createSliderdJointCreateSlider :: World ->
                                                                                   JointGroup ->
                                                                                   IO Joint
createContact :: World ->
                 Maybe JointGroup -> ContactInfo -> IO Joint
createContact = \arg_0 arg_1 arg_2 -> (\action_3 -> action_3 arg_0) (\marshaledArg_4 -> (case arg_1 of
                                                                                             Data.Maybe.Just a_5 -> \action_6 -> action_6 a_5
                                                                                             Data.Maybe.Nothing -> \action_7 -> action_7 nullPtr) (\marshaledArg_8 -> (\action_9 -> alloca (\ptr_10 -> do poke ptr_10 arg_2
                                                                                                                                                                                                          action_9 ptr_10)) (\marshaledArg_11 -> do ret_12 <- createContactdJointCreateContact marshaledArg_4 marshaledArg_8 marshaledArg_11
                                                                                                                                                                                                                                                    return (ret_12))))
foreign import ccall unsafe "dJointCreateContact" createContactdJointCreateContact :: World ->
                                                                                      JointGroup ->
                                                                                      Ptr ContactInfo ->
                                                                                      IO Joint
createGroup' :: Int -> IO JointGroup
createGroup' = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- createGroup'dJointGroupCreate marshaledArg_2
                                                                              return (ret_3))
foreign import ccall unsafe "dJointGroupCreate" createGroup'dJointGroupCreate :: Int ->
                                                                                 IO JointGroup
destroyJoint :: Joint -> IO ()
destroyJoint = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- destroyJointdJointDestroy marshaledArg_2
                                                                              case () of
                                                                                  () -> do return ())
foreign import ccall unsafe "dJointDestroy" destroyJointdJointDestroy :: Joint ->
                                                                         IO ()
destroyGroup :: JointGroup -> IO ()
destroyGroup = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- destroyGroupdJointGroupDestroy marshaledArg_2
                                                                              case () of
                                                                                  () -> do return ())
foreign import ccall unsafe "dJointGroupDestroy" destroyGroupdJointGroupDestroy :: JointGroup ->
                                                                                   IO ()
emptyGroup :: JointGroup -> IO ()
emptyGroup = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- emptyGroupdJointGroupEmpty marshaledArg_2
                                                                            case () of
                                                                                () -> do return ())
foreign import ccall unsafe "dJointGroupEmpty" emptyGroupdJointGroupEmpty :: JointGroup ->
                                                                             IO ()
attach :: Joint -> Maybe Body -> Maybe Body -> IO ()
attach = \arg_0 arg_1 arg_2 -> (\action_3 -> action_3 arg_0) (\marshaledArg_4 -> (case arg_1 of
                                                                                      Data.Maybe.Just a_5 -> \action_6 -> action_6 a_5
                                                                                      Data.Maybe.Nothing -> \action_7 -> action_7 nullPtr) (\marshaledArg_8 -> (case arg_2 of
                                                                                                                                                                    Data.Maybe.Just a_9 -> \action_10 -> action_10 a_9
                                                                                                                                                                    Data.Maybe.Nothing -> \action_11 -> action_11 nullPtr) (\marshaledArg_12 -> do ret_13 <- attachdJointAttach marshaledArg_4 marshaledArg_8 marshaledArg_12
                                                                                                                                                                                                                                                   case () of
                                                                                                                                                                                                                                                       () -> do return ())))
foreign import ccall unsafe "dJointAttach" attachdJointAttach :: Joint ->
                                                                 Body -> Body -> IO ()
getType :: Joint -> IO JointType
getType = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getTypedJointGetType marshaledArg_2
                                                                         return (toJointType (ret_3)))
foreign import ccall unsafe "dJointGetType" getTypedJointGetType :: Joint ->
                                                                    IO Int
getBody :: Joint -> BodyIndex -> IO Body
getBody = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 (fromBodyIndex arg_1)) (\marshaledArg_5 -> do ret_6 <- getBodydJointGetBody marshaledArg_3 marshaledArg_5
                                                                                                                                                 return (ret_6)))
foreign import ccall unsafe "dJointGetBody" getBodydJointGetBody :: Joint ->
                                                                    Int -> IO Body
areConnected :: Body -> Body -> IO Bool
areConnected = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 arg_1) (\marshaledArg_5 -> do ret_6 <- areConnecteddAreConnected marshaledArg_3 marshaledArg_5
                                                                                                                                      return (toBool (ret_6))))
foreign import ccall unsafe "dAreConnected" areConnecteddAreConnected :: Body ->
                                                                         Body -> IO Int
areConnectedExcluding :: Body -> Body -> JointType -> IO Bool
areConnectedExcluding = \arg_0 arg_1 arg_2 -> (\action_3 -> action_3 arg_0) (\marshaledArg_4 -> (\action_5 -> action_5 arg_1) (\marshaledArg_6 -> (\action_7 -> action_7 (fromJointType arg_2)) (\marshaledArg_8 -> do ret_9 <- areConnectedExcludingdAreConnectedExcluding marshaledArg_4 marshaledArg_6 marshaledArg_8
                                                                                                                                                                                                                       return (toBool (ret_9)))))
foreign import ccall unsafe "dAreConnectedExcluding" areConnectedExcludingdAreConnectedExcluding :: Body ->
                                                                                                    Body ->
                                                                                                    Int ->
                                                                                                    IO Int
setBallAnchor :: Joint -> ODEreal -> ODEreal -> ODEreal -> IO ()
setBallAnchor = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setBallAnchordJointSetBallAnchor marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                          case () of
                                                                                                                                                                                                                                                              () -> do return ()))))
foreign import ccall unsafe "dJointSetBallAnchor" setBallAnchordJointSetBallAnchor :: Joint ->
                                                                                      ODEreal ->
                                                                                      ODEreal ->
                                                                                      ODEreal ->
                                                                                      IO ()
getBallAnchor :: Joint -> IO ((ODEreal, ODEreal, ODEreal))
getBallAnchor = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> allocaArray 4 (\marshaledArg_3 -> do ret_4 <- getBallAnchordJointGetBallAnchor marshaledArg_2 marshaledArg_3
                                                                                                                 peekVector3 (marshaledArg_3)))
foreign import ccall unsafe "dJointGetBallAnchor" getBallAnchordJointGetBallAnchor :: Joint ->
                                                                                      Ptr ODEreal ->
                                                                                      IO ()
-- -------------------------------------------
--  Hinge joint
setHingeAnchor :: Joint -> ODEreal -> ODEreal -> ODEreal -> IO ()
setHingeAnchor = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setHingeAnchordJointSetHingeAnchor marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                           case () of
                                                                                                                                                                                                                                                               () -> do return ()))))
foreign import ccall unsafe "dJointSetHingeAnchor" setHingeAnchordJointSetHingeAnchor :: Joint ->
                                                                                         ODEreal ->
                                                                                         ODEreal ->
                                                                                         ODEreal ->
                                                                                         IO ()
setHingeAxis :: Joint -> ODEreal -> ODEreal -> ODEreal -> IO ()
setHingeAxis = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setHingeAxisdJointSetHingeAxis marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                         case () of
                                                                                                                                                                                                                                                             () -> do return ()))))
foreign import ccall unsafe "dJointSetHingeAxis" setHingeAxisdJointSetHingeAxis :: Joint ->
                                                                                   ODEreal ->
                                                                                   ODEreal ->
                                                                                   ODEreal -> IO ()
