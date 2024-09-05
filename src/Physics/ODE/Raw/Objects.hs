-- no bind missing, but these are not all tested
module Physics.ODE.Raw.Objects
    ( c'dCreateCCylinder
    , c'dCreateRay
    , c'dCreateSphere
    , c'dCreateBox
    , c'dCreatePlane
    , c'dGeomCCylinderSetParams
    , c'dGeomCCylinderGetParams
    , c'dGeomRaySet
    , c'dGeomRayGet
    , c'dGeomSphereSetRadius
    , c'dGeomSphereGetRadius
    , c'dGeomRaySetLength
    , c'dGeomRayGetLength
    , c'dGeomBoxSetLengths
    , c'dGeomBoxGetLengths
    , c'dGeomPlaneSetParams
    , c'dGeomPlaneGetParams
    , c'dGeomCCylinderPointDepth
    , c'dGeomSpherePointDepth
    , c'dGeomBoxPointDepth
    , c'dGeomPlanePointDepth
    )
where
import Foreign (Ptr)
import Physics.ODE.Raw.Types (Geom,Space)
foreign import ccall unsafe "dCreateCCylinder"          c'dCreateCCylinder          :: Space -> Float -> Float -> IO Geom
foreign import ccall unsafe "dCreateRay"                c'dCreateRay                :: Space -> Float -> IO Geom
foreign import ccall unsafe "dCreateSphere"             c'dCreateSphere             :: Space -> Float -> IO Geom
foreign import ccall unsafe "dCreateBox"                c'dCreateBox                :: Space -> Float -> Float -> Float -> IO Geom
foreign import ccall unsafe "dCreatePlane"              c'dCreatePlane              :: Space -> Float -> Float -> Float -> Float -> IO Geom
foreign import ccall unsafe "dGeomCCylinderSetParams"   c'dGeomCCylinderSetParams   :: Geom -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomCCylinderGetParams"   c'dGeomCCylinderGetParams   :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomRaySet"               c'dGeomRaySet               :: Geom -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomRayGet"               c'dGeomRayGet               :: Geom -> Ptr a -> IO ()
foreign import ccall unsafe "dGeomSphereSetRadius"      c'dGeomSphereSetRadius      :: Geom -> Float -> IO ()
foreign import ccall unsafe "dGeomSphereGetRadius"      c'dGeomSphereGetRadius      :: Geom -> IO Float
foreign import ccall unsafe "dGeomRaySetLength"         c'dGeomRaySetLength         :: Geom -> Float -> IO ()
foreign import ccall unsafe "dGeomRayGetLength"         c'dGeomRayGetLength         :: Geom -> IO Float
foreign import ccall unsafe "dGeomBoxSetLengths"        c'dGeomBoxSetLengths        :: Geom -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomBoxGetLengths"        c'dGeomBoxGetLengths        :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomPlaneSetParams"       c'dGeomPlaneSetParams       :: Geom -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomPlaneGetParams"       c'dGeomPlaneGetParams       :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomCCylinderPointDepth"  c'dGeomCCylinderPointDepth  :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dGeomSpherePointDepth"     c'dGeomSpherePointDepth     :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dGeomBoxPointDepth"        c'dGeomBoxPointDepth        :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dGeomPlanePointDepth"      c'dGeomPlanePointDepth      :: Geom -> Float -> Float -> Float -> IO Float
