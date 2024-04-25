module Physics.ODE.Geom
       (destroyGeom, setRawGeomData, setGeomData, setSafeGeomData,
        getRawGeomData, getGeomData, getSafeGeomData, tryGetSafeGeomData,
        setBody, getBodyUnsafe, getBody, setGeomPosition, getGeomPosition,
        setGeomQuaternion, getGeomQuaternion, setGeomRotation,
        getGeomRotation, isSpace, getSpace, getClass, enableGeom,
        disableGeom, isGeomEnabled)
       where
import Foreign
import Data.Typeable
import Data.Maybe
import Physics.ODE.Types
import Physics.ODE.Utilities
import Physics.ODE.Hsc
 
foreign import ccall unsafe "dGeomSetData" setRawGeomData ::
               Ptr GeomStruct -> Ptr a -> IO ()
 
setGeomData :: Geom -> a -> IO ()
setGeomData body d
  = newStablePtr d >>=
      \ stablePtr -> setRawGeomData body (castStablePtrToPtr stablePtr)
 
setSafeGeomData :: (Typeable a) => Geom -> a -> IO ()
setSafeGeomData body d = setGeomData body (typeOf d, d)
 
foreign import ccall unsafe "dGeomGetData" getRawGeomData ::
               Ptr GeomStruct -> IO (Ptr a)
 
getGeomData :: Geom -> IO a
getGeomData body
  = getRawGeomData body >>= deRefStablePtr . castPtrToStablePtr
 
tryGetSafeGeomData :: (Typeable a) => Geom -> IO (Maybe a)
tryGetSafeGeomData body
  = getGeomData body >>=
      \ (t, d) ->
        if t == typeOf d then return (Just d) else return Nothing
 
getSafeGeomData :: (Typeable a) => Geom -> IO a
getSafeGeomData
  = fmap (fromMaybe (error errMsg)) . tryGetSafeGeomData
  where errMsg = "Physics.ODE.Geom.getSafeGeomData: invalid type."
destroyGeom :: Geom -> IO ()
destroyGeom = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- destroyGeomdGeomDestroy marshaledArg_2
                                                                             case () of
                                                                                 () -> do return ())
foreign import ccall unsafe "dGeomDestroy" destroyGeomdGeomDestroy :: Geom ->
                                                                      IO ()
setBody :: Geom -> Maybe Body -> IO ()
setBody = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (case arg_1 of
                                                                                 Data.Maybe.Just a_4 -> \action_5 -> action_5 a_4
                                                                                 Data.Maybe.Nothing -> \action_6 -> action_6 nullPtr) (\marshaledArg_7 -> do ret_8 <- setBodydGeomSetBody marshaledArg_3 marshaledArg_7
                                                                                                                                                             case () of
                                                                                                                                                                 () -> do return ()))
foreign import ccall unsafe "dGeomSetBody" setBodydGeomSetBody :: Geom ->
                                                                  Body -> IO ()
getBodyUnsafe :: Geom -> IO Body
getBodyUnsafe = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getBodyUnsafedGeomGetBody marshaledArg_2
                                                                               return (ret_3))
foreign import ccall unsafe "dGeomGetBody" getBodyUnsafedGeomGetBody :: Geom ->
                                                                        IO Body
getBody :: Geom -> IO (Maybe Body)
getBody = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getBodydGeomGetBody marshaledArg_2
                                                                         if (ret_3) == nullPtr
                                                                          then return Nothing
                                                                          else fmap Just (return (ret_3)))
foreign import ccall unsafe "dGeomGetBody" getBodydGeomGetBody :: Geom ->
                                                                  IO Body
setGeomPosition :: Geom -> ODEreal -> ODEreal -> ODEreal -> IO ()
setGeomPosition = \arg_0 arg_1 arg_2 arg_3 -> (\action_4 -> action_4 arg_0) (\marshaledArg_5 -> (\action_6 -> action_6 arg_1) (\marshaledArg_7 -> (\action_8 -> action_8 arg_2) (\marshaledArg_9 -> (\action_10 -> action_10 arg_3) (\marshaledArg_11 -> do ret_12 <- setGeomPositiondGeomSetPosition marshaledArg_5 marshaledArg_7 marshaledArg_9 marshaledArg_11
                                                                                                                                                                                                                                                            case () of
                                                                                                                                                                                                                                                                () -> do return ()))))
foreign import ccall unsafe "dGeomSetPosition" setGeomPositiondGeomSetPosition :: Geom ->
                                                                                  ODEreal ->
                                                                                  ODEreal ->
                                                                                  ODEreal -> IO ()
getGeomPosition :: Geom -> IO ((ODEreal, ODEreal, ODEreal))
getGeomPosition = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getGeomPositiondGeomGetPosition marshaledArg_2
                                                                                 peekVector3 (ret_3))
foreign import ccall unsafe "dGeomGetPosition" getGeomPositiondGeomGetPosition :: Geom ->
                                                                                  IO (Ptr ODEreal)
setGeomQuaternion :: Geom ->
                     (ODEreal, ODEreal, ODEreal, ODEreal) -> IO ()
setGeomQuaternion = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> allocaArray 4 (\ptr_5 -> (>>) (pokeArray ptr_5 (case arg_1 of
                                                                                                                                                        (a_6,
                                                                                                                                                         b_7,
                                                                                                                                                         c_8,
                                                                                                                                                         d_9) -> [a_6,
                                                                                                                                                                  b_7,
                                                                                                                                                                  c_8,
                                                                                                                                                                  d_9])) (action_4 ptr_5))) (\marshaledArg_10 -> do ret_11 <- setGeomQuaterniondGeomSetQuaternion marshaledArg_3 marshaledArg_10
                                                                                                                                                                                                                    case () of
                                                                                                                                                                                                                        () -> do return ()))
foreign import ccall unsafe "dGeomSetQuaternion" setGeomQuaterniondGeomSetQuaternion :: Geom ->
                                                                                        Ptr ODEreal ->
                                                                                        IO ()
getGeomQuaternion :: Geom ->
                     IO ((ODEreal, ODEreal, ODEreal, ODEreal))
getGeomQuaternion = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> allocaArray 4 (\marshaledArg_3 -> do ret_4 <- getGeomQuaterniondGeomGetQuaternion marshaledArg_2 marshaledArg_3
                                                                                                                     peekVector4 (marshaledArg_3)))
foreign import ccall unsafe "dGeomGetQuaternion" getGeomQuaterniondGeomGetQuaternion :: Geom ->
                                                                                        Ptr ODEreal ->
                                                                                        IO ()
setGeomRotation :: Geom -> Matrix3 -> IO ()
setGeomRotation = \arg_0 arg_1 -> (\action_2 -> action_2 arg_0) (\marshaledArg_3 -> (\action_4 -> action_4 arg_1) (\marshaledArg_5 -> do ret_6 <- setGeomRotationdGeomSetRotation marshaledArg_3 marshaledArg_5
                                                                                                                                         case () of
                                                                                                                                             () -> do return ()))
foreign import ccall unsafe "dGeomSetRotation" setGeomRotationdGeomSetRotation :: Geom ->
                                                                                  Matrix3 -> IO ()
getGeomRotation :: Geom -> IO Matrix3
getGeomRotation = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getGeomRotationdGeomGetRotation marshaledArg_2
                                                                                 return (ret_3))
foreign import ccall unsafe "dGeomGetRotation" getGeomRotationdGeomGetRotation :: Geom ->
                                                                                  IO Matrix3
isSpace :: Geom -> IO Bool
isSpace = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- isSpacedGeomIsSpace marshaledArg_2
                                                                         return (toBool (ret_3)))
foreign import ccall unsafe "dGeomIsSpace" isSpacedGeomIsSpace :: Geom ->
                                                                  IO Int
getSpace :: Geom -> IO (Maybe Space)
getSpace = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getSpacedGeomGetSpace marshaledArg_2
                                                                          if (ret_3) == nullPtr
                                                                           then return Nothing
                                                                           else fmap Just (return (ret_3)))
foreign import ccall unsafe "dGeomGetSpace" getSpacedGeomGetSpace :: Geom ->
                                                                     IO Space
getClass :: Geom -> IO GeomClass
getClass = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- getClassdGeomGetClass marshaledArg_2
                                                                          return (toGeomClass (ret_3)))
foreign import ccall unsafe "dGeomGetClass" getClassdGeomGetClass :: Geom ->
                                                                     IO Int
enableGeom :: Geom -> IO ()
enableGeom = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- enableGeomdGeomEnable marshaledArg_2
                                                                            case () of
                                                                                () -> do return ())
foreign import ccall unsafe "dGeomEnable" enableGeomdGeomEnable :: Geom ->
                                                                   IO ()
disableGeom :: Geom -> IO ()
disableGeom = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- disableGeomdGeomDisable marshaledArg_2
                                                                             case () of
                                                                                 () -> do return ())
foreign import ccall unsafe "dGeomDisable" disableGeomdGeomDisable :: Geom ->
                                                                      IO ()
isGeomEnabled :: Geom -> IO Bool
isGeomEnabled = \arg_0 -> (\action_1 -> action_1 arg_0) (\marshaledArg_2 -> do ret_3 <- isGeomEnableddGeomIsEnabled marshaledArg_2
                                                                               return (toBool (ret_3)))
foreign import ccall unsafe "dGeomIsEnabled" isGeomEnableddGeomIsEnabled :: Geom ->
                                                                            IO Int
