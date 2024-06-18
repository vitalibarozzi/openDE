module Physics.ODE.Raw.Objects where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dCreateSphere"
    createSpheredCreateSphere ::
        Space ->
        ODEreal ->
        IO Geom
foreign import ccall unsafe "dGeomSphereSetRadius"
    sphereSetRadiusdGeomSphereSetRadius ::
        Geom ->
        ODEreal ->
        IO ()
foreign import ccall unsafe "dGeomSphereGetRadius"
    sphereGetRadiusdGeomSphereGetRadius ::
        Geom ->
        IO ODEreal
foreign import ccall unsafe "dGeomSpherePointDepth"
    spherePointDepthdGeomSpherePointDepth ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ODEreal
foreign import ccall unsafe "dCreateBox"
    createBoxdCreateBox ::
        Space ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO Geom
foreign import ccall unsafe "dGeomBoxSetLengths"
    boxSetLengthsdGeomBoxSetLengths ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ()
foreign import ccall unsafe "dGeomBoxGetLengths"
    boxGetLengthsdGeomBoxGetLengths ::
        Geom ->
        Ptr ODEreal ->
        IO ()
foreign import ccall unsafe "dGeomBoxPointDepth"
    boxPointDepthdGeomBoxPointDepth ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ODEreal
foreign import ccall unsafe "dCreatePlane"
    createPlanedCreatePlane ::
        Space ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO Geom
foreign import ccall unsafe "dGeomPlaneSetParams"
    planeSetParamsdGeomPlaneSetParams ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ()
foreign import ccall unsafe "dGeomPlaneGetParams"
    planeGetParamsdGeomPlaneGetParams ::
        Geom ->
        Ptr ODEreal ->
        IO ()
foreign import ccall unsafe "dGeomPlanePointDepth"
    planePointDepthdGeomPlanePointDepth ::
        Geom ->
        ODEreal ->
        ODEreal ->
        ODEreal ->
        IO ODEreal
