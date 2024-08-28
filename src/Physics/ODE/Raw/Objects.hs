module Physics.ODE.Raw.Objects where
import Foreign
import Physics.ODE.Raw.Types

-- TODO check if anyone missing
-- 10.7.4. Capped Cylinder Class

--dGeomID dCreateCCylinder (dSpaceID space, dReal radius, dReal length);
--void dGeomCCylinderSetParams (dGeomID ccylinder,dReal radius, dReal length);
--void dGeomCCylinderGetParams (dGeomID ccylinder,dReal *radius, dReal *length);
--dReal dGeomCCylinderPointDepth (dGeomID ccylinder,dReal x, dReal y, dReal z);
--dGeomID dCreateRay (dSpaceID space, dReal length);
--void dGeomRaySetLength (dGeomID ray, dReal length);
--dReal dGeomRayGetLength (dGeomID ray);
--void dGeomRaySet (dGeomID ray, dReal px, dReal py, dReal pz,dReal dx, dReal dy, dReal dz);
--void dGeomRayGet (dGeomID ray, dVector3 start, dVector3 dir);

foreign import ccall unsafe "dCreateSphere"          c'dCreateSphere           :: Space -> Float -> IO Geom
foreign import ccall unsafe "dCreateBox"             c'dCreateBox              :: Space -> Float -> Float -> Float -> IO Geom
foreign import ccall unsafe "dCreatePlane"           c'dCreatePlane            :: Space -> Float -> Float -> Float -> Float -> IO Geom


foreign import ccall unsafe "dGeomSphereSetRadius"   c'dGeomSphereSetRadius    :: Geom -> Float -> IO ()
foreign import ccall unsafe "dGeomSphereGetRadius"   c'dGeomSphereGetRadius    :: Geom -> IO Float

foreign import ccall unsafe "dGeomBoxSetLengths"     c'dGeomBoxSetLengths      :: Geom -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomBoxGetLengths"     c'dGeomBoxGetLengths      :: Geom -> Ptr Float -> IO ()

foreign import ccall unsafe "dGeomPlaneSetParams"    c'dGeomPlaneSetParams     :: Geom -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomPlaneGetParams"    c'dGeomPlaneGetParams     :: Geom -> Ptr Float -> IO ()

foreign import ccall unsafe "dGeomSpherePointDepth"  c'dGeomSpherePointDepth   :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dGeomBoxPointDepth"     c'dGeomBoxPointDepth      :: Geom -> Float -> Float -> Float -> IO Float
foreign import ccall unsafe "dGeomPlanePointDepth"   c'dGeomPlanePointDepth    :: Geom -> Float -> Float -> Float -> IO Float
