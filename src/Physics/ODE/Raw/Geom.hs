module Physics.ODE.Raw.Geom where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dGeomSetData" setRawGeomData :: Ptr GeomStruct -> Ptr a -> IO ()
foreign import ccall unsafe "dGeomGetData" getRawGeomData :: Ptr GeomStruct -> IO (Ptr a)
foreign import ccall unsafe "dGeomDestroy" destroyGeomdGeomDestroy :: Geom -> IO ()
foreign import ccall unsafe "dGeomSetBody" setBodydGeomSetBody :: Geom -> Body -> IO ()
foreign import ccall unsafe "dGeomGetBody" getBodyUnsafedGeomGetBody :: Geom -> IO Body
foreign import ccall unsafe "dGeomGetBody" getBodydGeomGetBody :: Geom -> IO Body
foreign import ccall unsafe "dGeomSetPosition" setGeomPositiondGeomSetPosition :: Geom -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomGetPosition" getGeomPositiondGeomGetPosition :: Geom -> IO (Ptr Float)
foreign import ccall unsafe "dGeomSetQuaternion" setGeomQuaterniondGeomSetQuaternion :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomGetQuaternion" getGeomQuaterniondGeomGetQuaternion :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomSetRotation" setGeomRotationdGeomSetRotation :: Geom -> Matrix3 -> IO ()
foreign import ccall unsafe "dGeomGetRotation" getGeomRotationdGeomGetRotation :: Geom -> IO Matrix3
foreign import ccall unsafe "dGeomIsSpace" isSpacedGeomIsSpace :: Geom -> IO Int
foreign import ccall unsafe "dGeomGetSpace" getSpacedGeomGetSpace :: Geom -> IO Space
foreign import ccall unsafe "dGeomGetClass" getClassdGeomGetClass :: Geom -> IO Int
foreign import ccall unsafe "dGeomEnable" enableGeomdGeomEnable :: Geom -> IO ()
foreign import ccall unsafe "dGeomDisable" disableGeomdGeomDisable :: Geom -> IO ()
foreign import ccall unsafe "dGeomIsEnabled" isGeomEnableddGeomIsEnabled :: Geom -> IO Int
