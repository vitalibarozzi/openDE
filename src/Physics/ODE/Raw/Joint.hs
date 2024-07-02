module Physics.ODE.Raw.Joint where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dJointSetData" c'setRawJointData :: Ptr JointStruct -> Ptr a -> IO ()
foreign import ccall unsafe "dJointGetData" c'getRawJointData :: Ptr JointStruct -> IO (Ptr a)
foreign import ccall unsafe "dJointCreateBall" c'createBalldJointCreateBall :: World -> JointGroup -> IO Joint
foreign import ccall unsafe "dJointCreateHinge" c'createHingedJointCreateHinge :: World -> JointGroup -> IO Joint
foreign import ccall unsafe "dJointCreateSlider" c'createSliderdJointCreateSlider :: World -> JointGroup -> IO Joint
foreign import ccall unsafe "dJointCreateContact" c'createContactdJointCreateContact :: World -> JointGroup -> Ptr ContactInfo -> IO Joint
foreign import ccall unsafe "dJointGroupCreate" c'createGroup'dJointGroupCreate :: Int -> IO JointGroup
foreign import ccall unsafe "dJointDestroy" c'destroyJointdJointDestroy :: Joint -> IO ()
foreign import ccall unsafe "dJointGroupDestroy" c'destroyGroupdJointGroupDestroy :: JointGroup -> IO ()
foreign import ccall unsafe "dJointGroupEmpty" c'emptyGroupdJointGroupEmpty :: JointGroup -> IO ()
foreign import ccall unsafe "dJointAttach" c'attachdJointAttach :: Joint -> Body -> Body -> IO ()
foreign import ccall unsafe "dJointGetType" c'getTypedJointGetType :: Joint -> IO Int
foreign import ccall unsafe "dJointGetBody" c'getBodydJointGetBody :: Joint -> Int -> IO Body
foreign import ccall unsafe "dAreConnected" c'areConnecteddAreConnected :: Body -> Body -> IO Int
foreign import ccall unsafe "dAreConnectedExcluding" c'areConnectedExcludingdAreConnectedExcluding :: Body -> Body -> Int -> IO Int
foreign import ccall unsafe "dJointSetBallAnchor" c'setBallAnchordJointSetBallAnchor :: Joint -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dJointGetBallAnchor" c'getBallAnchordJointGetBallAnchor :: Joint -> Ptr Float -> IO ()
foreign import ccall unsafe "dJointSetHingeAnchor" c'setHingeAnchordJointSetHingeAnchor :: Joint -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dJointSetHingeAxis" c'setHingeAxisdJointSetHingeAxis :: Joint -> Float -> Float -> Float -> IO ()

