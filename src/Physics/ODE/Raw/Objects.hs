module Physics.ODE.Raw.Objects where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dCreateSphere" createSpheredCreateSphere :: Space -> Float -> IO Geom
foreign import ccall unsafe "dGeomSphereSetRadius" sphereSetRadiusdGeomSphereSetRadius :: Geom -> Float -> IO ()
foreign import ccall unsafe "dGeomSphereGetRadius" sphereGetRadiusdGeomSphereGetRadius :: Geom -> IO Float
foreign import ccall unsafe "dGeomSpherePointDepth" spherePointDepthdGeomSpherePointDepth :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dCreateBox" createBoxdCreateBox :: Space -> Float -> Float -> Float -> IO Geom
foreign import ccall unsafe "dGeomBoxSetLengths" boxSetLengthsdGeomBoxSetLengths :: Geom -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomBoxGetLengths" boxGetLengthsdGeomBoxGetLengths :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomBoxPointDepth" boxPointDepthdGeomBoxPointDepth :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dCreatePlane" createPlanedCreatePlane :: Space -> Float -> Float -> Float -> Float -> IO Geom
foreign import ccall unsafe "dGeomPlaneSetParams" planeSetParamsdGeomPlaneSetParams :: Geom -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomPlaneGetParams" planeGetParamsdGeomPlaneGetParams :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomPlanePointDepth" planePointDepthdGeomPlanePointDepth :: Geom -> Float -> Float -> Float -> IO Float
