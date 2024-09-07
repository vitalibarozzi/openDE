-- | 100% complete.
module Physics.ODE.Raw.Objects
    ( c'dCreateBox
    , c'dCreateCapsule
    , c'dCreateCylinder
    , c'dCreatePlane
    , c'dCreateRay
    , c'dCreateSphere
    , c'dGeomBoxGetLengths
    , c'dGeomBoxPointDepth
    , c'dGeomBoxSetLengths
    , c'dGeomCapsuleGetParams
    , c'dGeomCapsulePointDepth
    , c'dGeomCapsuleSetParams
    , c'dGeomCylinderGetParams
    , c'dGeomCylinderSetParams
    , c'dGeomPlaneGetParams
    , c'dGeomPlanePointDepth
    , c'dGeomPlaneSetParams
    , c'dGeomRayGet
    , c'dGeomRayGetClosestHit
    , c'dGeomRayGetLength
    , c'dGeomRayGetParams
    , c'dGeomRaySet
    , c'dGeomRaySetClosestHit
    , c'dGeomRaySetLength
    , c'dGeomRaySetParams
    , c'dGeomSphereGetRadius
    , c'dGeomSpherePointDepth
    , c'dGeomSphereSetRadius
    )
where
import Foreign (Ptr)
import Physics.ODE.Raw.Types (Geom,Space)
foreign import ccall unsafe "dCreateBox"                c'dCreateBox                :: Space -> Float -> Float -> Float -> IO Geom
foreign import ccall unsafe "dCreateCapsule"            c'dCreateCapsule            :: Space -> Float -> Float -> IO Geom
foreign import ccall unsafe "dCreateCylinder"           c'dCreateCylinder           :: Space -> Float -> Float -> IO Geom
foreign import ccall unsafe "dCreatePlane"              c'dCreatePlane              :: Space -> Float -> Float -> Float -> Float -> IO Geom
foreign import ccall unsafe "dCreateRay"                c'dCreateRay                :: Space -> Float -> IO Geom
foreign import ccall unsafe "dCreateSphere"             c'dCreateSphere             :: Space -> Float -> IO Geom
foreign import ccall unsafe "dGeomBoxGetLengths"        c'dGeomBoxGetLengths        :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomBoxPointDepth"        c'dGeomBoxPointDepth        :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dGeomBoxSetLengths"        c'dGeomBoxSetLengths        :: Geom -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomCapsuleGetParams"     c'dGeomCapsuleGetParams     :: Geom -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomCapsulePointDepth"    c'dGeomCapsulePointDepth    :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dGeomCapsuleSetParams"     c'dGeomCapsuleSetParams     :: Geom -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomCylinderGetParams"    c'dGeomCylinderGetParams    :: Geom -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomCylinderSetParams"    c'dGeomCylinderSetParams    :: Geom -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomPlaneGetParams"       c'dGeomPlaneGetParams       :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomPlanePointDepth"      c'dGeomPlanePointDepth      :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dGeomPlaneSetParams"       c'dGeomPlaneSetParams       :: Geom -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomRayGet"               c'dGeomRayGet               :: Geom -> Ptr Float -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomRayGetClosestHit"     c'dGeomRayGetClosestHit     :: Geom -> IO Int
foreign import ccall unsafe "dGeomRayGetLength"         c'dGeomRayGetLength         :: Geom -> IO Float
foreign import ccall unsafe "dGeomRayGetParams"         c'dGeomRayGetParams         :: Geom -> Ptr Int -> Ptr Int -> IO ()
foreign import ccall unsafe "dGeomRaySet"               c'dGeomRaySet               :: Geom -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomRaySetClosestHit"     c'dGeomRaySetClosestHit     :: Geom -> Int -> IO ()
foreign import ccall unsafe "dGeomRaySetLength"         c'dGeomRaySetLength         :: Geom -> Float -> IO ()
foreign import ccall unsafe "dGeomRaySetParams"         c'dGeomRaySetParams         :: Geom -> Int -> Int -> IO ()
foreign import ccall unsafe "dGeomSphereGetRadius"      c'dGeomSphereGetRadius      :: Geom -> IO Float
foreign import ccall unsafe "dGeomSpherePointDepth"     c'dGeomSpherePointDepth     :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dGeomSphereSetRadius"      c'dGeomSphereSetRadius      :: Geom -> Float -> IO ()
