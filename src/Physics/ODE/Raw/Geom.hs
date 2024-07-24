module Physics.ODE.Raw.Geom where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dGeomSetData" setRawGeomData                            :: Ptr GeomStruct -> Ptr a -> IO ()
foreign import ccall unsafe "dGeomGetData" getRawGeomData                            :: Ptr GeomStruct -> IO (Ptr a)
foreign import ccall unsafe "dGeomDestroy" destroyGeomdGeomDestroy                   :: Geom -> IO ()
foreign import ccall unsafe "dGeomSetBody" setBodydGeomSetBody                       :: Geom -> Body -> IO ()
foreign import ccall unsafe "dGeomGetBody" getBodyUnsafedGeomGetBody                 :: Geom -> IO Body
foreign import ccall unsafe "dGeomGetBody" getBodydGeomGetBody                       :: Geom -> IO Body
foreign import ccall unsafe "dGeomSetPosition" setGeomPositiondGeomSetPosition       :: Geom -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dGeomGetPosition" getGeomPositiondGeomGetPosition       :: Geom -> IO (Ptr Float)
foreign import ccall unsafe "dGeomSetQuaternion" setGeomQuaterniondGeomSetQuaternion :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomGetQuaternion" getGeomQuaterniondGeomGetQuaternion :: Geom -> Ptr Float -> IO ()
foreign import ccall unsafe "dGeomSetRotation" setGeomRotationdGeomSetRotation       :: Geom -> Matrix3 -> IO ()
foreign import ccall unsafe "dGeomGetRotation" getGeomRotationdGeomGetRotation       :: Geom -> IO Matrix3
foreign import ccall unsafe "dGeomIsSpace" isSpacedGeomIsSpace                       :: Geom -> IO Int
foreign import ccall unsafe "dGeomGetSpace" getSpacedGeomGetSpace                    :: Geom -> IO Space
foreign import ccall unsafe "dGeomGetClass" getClassdGeomGetClass                    :: Geom -> IO Int
foreign import ccall unsafe "dGeomEnable" enableGeomdGeomEnable                      :: Geom -> IO ()
foreign import ccall unsafe "dGeomDisable" disableGeomdGeomDisable                   :: Geom -> IO ()
foreign import ccall unsafe "dGeomIsEnabled" isGeomEnableddGeomIsEnabled             :: Geom -> IO Int

{-
void dGeomDestroy (dGeomID);

Destroy a geom, removing it from any space it is in first. This one function destroys a geom of any type, but to create a geom you must call a creation function for that type.

When a space is destroyed, if its cleanup mode is 1 (the default) then all the geoms in that space are automatically destroyed as well.

void dGeomSetData (dGeomID, void *);
void * dGeomGetData (dGeomID);

These functions set and get the user-defined data pointer stored in the geom.

void dGeomSetBody (dGeomID, dBodyID);
dBodyID dGeomGetBody (dGeomID);

These functions set and get the body associated with a placeable geom. Setting a body on a geom automatically combines the position vector and rotation matrix of the body and geom, so that setting the position or orientation of one will set the value for both objects.

Setting a body ID of zero gives the geom its own position and rotation, independent from any body. If the geom was previously connected to a body then its new independent position/rotation is set to the current position/rotation of the body.

Calling these functions on a non-placeable geom results in a runtime error in the debug build of ODE.

void dGeomSetPosition (dGeomID, dReal x, dReal y, dReal z);
void dGeomSetRotation (dGeomID, const dMatrix3 R);
void dGeomSetQuaternion (dGeomID, const dQuaternion);

Set the position vector, rotation matrix or quaternion of a placeable geom. These functions are analogous to dBodySetPosition, dBodySetRotation and dBodySetQuaternion. If the geom is attached to a body, the body's position / rotation / quaternion will also be changed.

Calling these functions on a non-placeable geom results in a runtime error in the debug build of ODE.

const dReal * dGeomGetPosition (dGeomID);
const dReal * dGeomGetRotation (dGeomID);
void dGeomGetQuaternion (dGeomID, dQuaternion result);

The first two return pointers to the geom's position vector and rotation matrix. The returned values are pointers to internal data structures, so the vectors are valid until any changes are made to the geom. If the geom is attached to a body, the body's position / rotation pointers will be returned, i.e. the result will be identical to calling dBodyGetPosition or dBodyGetRotation.

dGeomGetQuaternion copies the geom's quaternion into the space provided. If the geom is attached to a body, the body's quaternion will be returned, i.e. the resulting quaternion will be the same as the result of calling dBodyGetQuaternion.

Calling these functions on a non-placeable geom results in a runtime error in the debug build of ODE.

void dGeomSetOffsetPosition (dGeomID, dReal x, dReal y, dReal z);
void dGeomSetOffsetRotation (dGeomID, const dMatrix3 R);
void dGeomSetOffsetQuaternion (dGeomID, const dQuaternion Q);

Set the offset position, rotation or quaternion of a geom. The geom must be attached to a body. If the geom did not have an offset, it is automatically created. This sets up an additional (local) transformation for the geom, since geoms attached to a body share their global position and rotation. To disable the offset call dGeomClearOffset.

void dGeomSetOffsetWorldPosition (dGeomID, dReal x, dReal y, dReal z);
void dGeomSetOffsetWorldRotation (dGeomID, const dMatrix3 R);
void dGeomSetOffsetWorldQuaternion (dGeomID, const dQuaternion Q);

Set the offset world position, rotation or quaternion of a geom. The new local offset is the difference of the current body transformation, so that the geom is placed and oriented in the world as specified, without changing the body's transformation. See also the previous three offset functions.

const dReal * dGeomGetOffsetPosition (dGeomID);
const dReal * dGeomGetOffsetRotation (dGeomID);
void dGeomGetOffsetQuaternion (dGeomID, dQuaternion result);

Get the offset position, rotation or quaternion of a geom. The returned value of the first two functions are pointers to the geom's internal data structure. They are valid until any changes are made to the geom. If the geom has no offset the zero vector is returned, in case of dGeomGetOffsetQuaternion the identity quaternion is returned.

void dGeomClearOffset (dGeomID);

Disable the geom's offset. The geom will be repositioned / oriented at the body's position / orientation. If the geom has no offset, this function does nothing. Note, that this will eliminate the offset and is more efficient than setting the offset to the identity transformation.

void dGeomGetAABB (dGeomID, dReal aabb[6]);

Return in aabb an axis aligned bounding box that surrounds the given geom. The aabb array has elements (minx, maxx, miny, maxy, minz, maxz). If the geom is a space, a bounding box that surrounds all contained geoms is returned.

This function may return a pre-computed cached bounding box, if it can determine that the geom has not moved since the last time the bounding box was computed.

int dGeomIsSpace (dGeomID);

Return 1 if the given geom is a space, or 0 if not.

dSpaceID dGeomGetSpace (dGeomID);

Return the space that the given geometry is contained in, or return 0 if it is not contained in any space.

int dGeomGetClass (dGeomID);

void dGeomSetCategoryBits (dGeomID, unsigned long bits);
void dGeomSetCollideBits (dGeomID, unsigned long bits);
unsigned long dGeomGetCategoryBits (dGeomID);
unsigned long dGeomGetCollideBits (dGeomID);

Set and get the "category" and "collide" bitfields for the given geom. These bitfields are use by spaces to govern which geoms will interact with each other. The bit fields are guaranteed to be at least 32 bits wide. The default category and collide values for newly created geoms have all bits set.

void dGeomEnable (dGeomID);
void dGeomDisable (dGeomID);
int dGeomIsEnabled (dGeomID);

void dGeomSphereSetRadius (dGeomID sphere, dReal radius);

Set the radius of the given sphere.

dReal dGeomSphereGetRadius (dGeomID sphere);

Return the radius of the given sphere.

dReal dGeomSpherePointDepth (dGeomID sphere, dReal x, dReal y, dReal z);

Return the depth of the point (x,y,z) in the given sphere. Points inside the geom will have positive depth, points outside it will have negative depth, and points on the surface will have zero depth.
Box Class

dGeomID dCreateBox (dSpaceID space, dReal lx, dReal ly, dReal lz);

Create a box geom of the given x/y/z side lengths (lx,ly,lz), and return its ID. If space is nonzero, insert it into that space. The point of reference for a box is its center.

void dGeomBoxSetLengths (dGeomID box, dReal lx, dReal ly, dReal lz);

Set the side lengths of the given box.

void dGeomBoxGetLengths (dGeomID box, dVector3 result);

Return in result the side lengths of the given box.

dReal dGeomBoxPointDepth (dGeomID box, dReal x, dReal y, dReal z);

Return the depth of the point (x,y,z) in the given box. Points inside the geom will have positive depth, points outside it will have negative depth, and points on the surface will have zero depth.
Plane Class

dGeomID dCreatePlane (dSpaceID space, dReal a, dReal b, dReal c, dReal d);

Create a plane geom of the given parameters, and return its ID. If space is nonzero, insert it into that space. The plane equation is

a*x+b*y+c*z = d The plane's normal vector is (a,b,c), and it must have length 1. Planes are non-placeable geoms. This means that, unlike placeable geoms, planes do not have an assigned position and rotation. This means that the parameters (a,b,c,d) are always in global coordinates. In other words it is assumed that the plane is always part of the static environment and not tied to any movable object.

void dGeomPlaneSetParams (dGeomID plane, dReal a, dReal b, dReal c, dReal d);

Set the parameters of the given plane.

void dGeomPlaneGetParams (dGeomID plane, dVector4 result);

Return in result the parameters of the given plane.

dReal dGeomPlanePointDepth (dGeomID plane, dReal x, dReal y, dReal z);

Return the depth of the point (x,y,z) in the given plane. Points inside the geom will have positive depth, points outside it will have negative depth, and points on the surface will have zero depth.

Note that planes in ODE are in fact not really planes: they are half-spaces. Anything that is moving inside the half-space will be ejected out from it. This means that planes are only planes from the perspective of one side. If you want your planes to be reversed, multiply the whole plane equation by -1.
Capsule Class

N.B. Capsule were called Capped Cylinder (CCylinder) before version 0.6

dGeomID dCreateCapsule (dSpaceID space, dReal radius, dReal length);

Create a capsule geom of the given parameters, and return its ID. If space is nonzero, insert it into that space.

A capsule is like a normal cylinder except it has half-sphere caps at its ends. This feature makes the internal collision detection code particularly fast and accurate. The cylinder's length, not counting the caps, is given by length. The cylinder is aligned along the geom's local Z axis. The radius of the caps, and of the cylinder itself, is given by radius.

void dGeomCapsuleSetParams (dGeomID capsule, dReal radius, dReal length);

Set the parameters of the given capsule.

void dGeomCapsuleGetParams (dGeomID capsule, dReal *radius, dReal *length);

Return in radius and length the parameters of the given capsule.

dReal dGeomCapsulePointDepth (dGeomID capsule, dReal x, dReal y, dReal z);

Return the depth of the point (x,y,z) in the given capsule. Points inside the geom will have positive depth, points outside it will have negative depth, and points on the surface will have zero depth.
Cylinder Class

dGeomID dCreateCylinder (dSpaceID space, dReal radius, dReal length);

Create a cylinder geom of the given parameters, and return its ID. If space is nonzero, insert it into that space.

void dGeomCylinderSetParams (dGeomID cylinder, dReal radius, dReal length);

Set the parameters of the given cylinder.

void dGeomCylinderGetParams (dGeomID cylinder, dReal *radius, dReal *length);

Return in radius and length the parameters of the given cylinder.
Ray Class

A ray is different from all the other geom classes in that it does not represent a solid object. It is an infinitely thin line that starts from the geom's position and extends in the direction of the geom's local Z-axis.

Calling dCollide between a ray and another geom will result in at most one contact point. Rays have their own conventions for the contact information in the dContactGeom structure (thus it is not useful to create contact joints from this information):

    pos - This is the point at which the ray intersects the surface of the other geom, regardless of whether the ray starts from inside or outside the geom.

    normal - This is the surface normal of the other geom at the contact point. if dCollide is passed the ray as its first geom then the normal will be oriented correctly for ray reflection from that surface (otherwise it will have the opposite sign).

    depth - This is the distance from the start of the ray to the contact point.

Rays are useful for things like visibility testing, determining the path of projectiles or light rays, and for object placement.

dGeomID dCreateRay (dSpaceID space, dReal length);

Create a ray geom of the given length, and return its ID. If space is nonzero, insert it into that space.

void dGeomRaySetLength (dGeomID ray, dReal length);

Set the length of the given ray.

dReal dGeomRayGetLength (dGeomID ray);

Get the length of the given ray.

void dGeomRaySet (dGeomID ray, dReal px, dReal py, dReal pz, dReal dx, dReal dy, dReal dz);

Set the starting position (px,py,pz) and direction (dx,dy,dz) of the given ray. The ray's rotation matrix will be adjusted so that the local Z-axis is aligned with the direction. Note that this does not adjust the ray's length.

void dGeomRayGet (dGeomID ray, dVector3 start, dVector3 dir);

Get the starting position (start) and direction (dir) of the ray. The returned direction will be a unit length vector.

void dGeomRaySetParams( dGeomID ray, int FirstContact, int BackfaceCull );
void dGeomRayGetParams( dGeomID ray, int *FirstContact, int *BackfaceCull );
void dGeomRaySetClosestHit( dGeomID ray, int ClosestHit );
int  dGeomRayGetClosestHit( dGeomID ray );

Set or Get parameters for the ray which determine which hit between the ray geom and a trimesh geom is returned from dCollide.

FirstContact determines if dCollide returns the first collision detected between the ray geom and a trimesh geom, even if that collision is not the nearest to the ray start position. BackfaceCull determines if dCollide returns a collision between the ray geom and a trimesh geom when the collision is between the ray and a backfacing triangle. Default values are FirstContact = 0, BackfaceCull = 0 (both false).

ClosestHit determines if dCollide returns the closest hit between the ray and a trimesh geom. If ClosestHit is false, the hit returned by dCollide may not be the nearest collision to the ray position. This parameter is ignored if FirstContact is set to true in dGeomRaySetParams(). If ClosestHit is set to true and BackfaceCull is set to false, the hit returned by dCollide may be between the ray and a backfacing triangle. The default value is ClosestHit = 0 (false).
Convex Class

dGeomID dCreateConvex (dSpaceID space, dReal *planes, unsigned planecount, 
                       dReal *points, unsigned pointcount, unsigned *polygons); 
void dGeomSetConvex (dGeomID g, dReal *planes, unsigned planecount,
                     dReal *points, unsigned pointcount, unsigned *polygons);

Triangle Mesh Class

A triangle mesh (TriMesh) represents an arbitrary collection of triangles. The triangle mesh collision system has the following features:

    Any triangle "soup" can be represented --- i.e. the triangles are not required to have any particular strip, fan or grid structure.
    Triangle meshes can interact with spheres, boxes, rays and other triangle meshes.
    It works well for relatively large triangles.
    It uses temporal coherence to speed up collision tests. When a geom has its collision checked with a trimesh once, data is stored inside the trimesh. This data can be cleared with the dGeomTriMeshClearTCCache function. In the future it will be possible to disable this functionality.

Trimesh/Trimesh collisions, perform quite well, but there are three minor caveats:

    The stepsize you use will, in general, have to be reduced for accurate collision resolution. Non-convex shape collision is much more dependent on the collision geometry than primitive collisions. Further, the local contact geometry will change more rapidly (and in a more complex fashion) for non-convex polytopes than it does for simple, convex polytopes such as spheres and cubes.

    In order to efficiently resolve collisions, dCollideTTL needs the positions of the colliding trimeshes in the previous timestep. This is used to calculate an estimated velocity of each colliding triangle, which is used to find the direction of impact, contact normals, etc. This requires the user to update these variables at every timestep. This update is performed outside of ODE, so it is not included in ODE itself. The code to do this looks something like this:

  const double *DoubleArrayPtr = Bodies(BodyIndex).TransformationMatrix->GetArray();
  dGeomTriMeshDataSet( TriMeshData, TRIMESH_LAST_TRANSFORMATION, (void *) DoubleArrayPtr );

The transformation matrix is the standard 4x4 homogeneous transform matrix, and the "DoubleArray" is the standard flattened array of the 16 matrix values.

NOTE: The triangle mesh class is not final, so in the future API changes might be expected.

NOTE: dInitODE() must have been called in order to successfully use Trimesh.

dTriMeshDataID dGeomTriMeshDataCreate();
void dGeomTriMeshDataDestroy (dTriMeshDataID g);

Creates and destroys a dTriMeshData object which is used to store mesh data.

void dGeomTriMeshDataBuild (dTriMeshDataID g,
                            const void* Vertices, int VertexStride, int VertexCount,
                            const void* Indices, int IndexCount, int TriStride,
                            const void* Normals);

NOTE: The argument order is non-intuitive here: stride,count for vertex data, but count,stride for index data.

Used for filling a dTriMeshData object with data. No data is copied here, so the pointers passed into this function must remain valid. This is how the strided data works:

struct StridedVertex {
    dVector3 Vertex; // 4th component can be left out, reducing memory usage
    // Userdata
}; 
int VertexStride = sizeof (StridedVertex);
struct StridedTri {
    int Indices(3);
    // Userdata
};
int TriStride = sizeof (StridedTri);

The Normals argument is optional: the normals of the faces of each trimesh object. For example,

dTriMeshDataID TriMeshData;
TriMeshData = dGeomTriMeshGetTriMeshDataID ( Bodies(BodyIndex).GeomID); // as long as dReal == floats
dGeomTriMeshDataBuildSingle (TriMeshData,
                             Bodies(BodyIndex).VertexPositions,  3*sizeof(dReal), (int) numVertices, // Vertices
                             Bodies(BodyIndex).TriangleIndices, 3*((int) NumTriangles), 3*sizeof(unsigned int), // Faces
                             Bodies(BodyIndex).FaceNormals); //  Normals

This pre-calculation saves some time during evaluation of the contacts, but isn't necessary. If you don't want to calculate the face normals before construction (or if you have enormous trimeshes and know that only very few faces will be touching and want to save time), just pass a "NULL" for the Normals argument, and dCollideTTL will take care of the normal calculations itself.

void dGeomTriMeshDataBuildSimple (dTriMeshDataID g,
                                  const dVector3*Vertices, int VertexCount,
                                  const int* Indices, int IndexCount);

Simple build function provided for convenience.

typedef int dTriCallback (dGeomID TriMesh, dGeomID RefObject, int TriangleIndex);
void dGeomTriMeshSetCallback (dGeomID g, dTriCallback *Callback);
dTriCallback* dGeomTriMeshGetCallback (dGeomID g);

Optional per triangle callback. Allows the user to say if collision with a particular triangle is wanted. If the return value is zero no contact will be generated.

typedef void dTriArrayCallback (dGeomID TriMesh, dGeomID RefObject, const int* TriIndices, int TriCount);
void dGeomTriMeshSetArrayCallback (dGeomID g, dTriArrayCallback* ArrayCallback);
dTriArrayCallback *dGeomTriMeshGetArrayCallback (dGeomID g);

Optional per geom callback. Allows the user to get the list of all intersecting triangles in one shot.

typedef int dTriRayCallback (dGeomID TriMesh, dGeomID Ray, int TriangleIndex, dReal u, dReal v);
void dGeomTriMeshSetRayCallback (dGeomID g, dTriRayCallback* Callback);
dTriRayCallback *dGeomTriMeshGetRayCallback (dGeomID g);

Optional Ray callback. Allows the user to determine if a ray collides with a triangle based on the barycentric coordinates of an intersection. The user can for example sample a bitmap to determine if a collision should occur.

dGeomID dCreateTriMesh (dSpaceID space, dTriMeshDataID Data,
                        dTriCallback *Callback,
                        dTriArrayCallback *ArrayCallback,
                        dTriRayCallback *RayCallback);

Constructor. The Data member defines the vertex data the newly created triangle mesh will use.

void dGeomTriMeshSetData (dGeomID g, dTriMeshDataID Data);

Replaces the current data.

void dGeomTriMeshClearTCCache (dGeomID g);

Clears the internal temporal coherence caches.

void dGeomTriMeshGetTriangle (dGeomID g, int Index, dVector3 *v0, dVector3 *v1, dVector3 *v2);

Retrieves a triangle in world space. The v0, v1 and v2 arguments are optional.

void dGeomTriMeshGetPoint (dGeomID g, int Index, dReal u, dReal v, dVector3 Out);

Retrieves a position in world space based on the incoming data.

void dGeomTriMeshEnableTC(dGeomID g, int geomClass, int enable);
int dGeomTriMeshIsTCEnabled(dGeomID g, int geomClass);

These functions can be used to enable/disable the use of temporal coherence during tri-mesh collision checks. Temporal coherence can be enabled/disabled per tri-mesh instance/geom class pair, currently it works for spheres and boxes. The default for spheres and boxes is 'false'.

The 'enable' param should be 1 for true, 0 for false.

Temporal coherence is optional because allowing it can cause subtle efficiency problems in situations where a tri-mesh may collide with many different geoms during its lifespan. If you enable temporal coherence on a tri-mesh then these problems can be eased by intermittently calling dGeomTriMeshClearTCCache for it.

typedef int dTriTriMergeCallback(dGeomID TriMesh, int FirstTriangleIndex, int SecondTriangleIndex);
void dGeomTriMeshSetTriMergeCallback(dGeomID g, dTriTriMergeCallback* Callback);
dTriTriMergeCallback* dGeomTriMeshGetTriMergeCallback(dGeomID g);

Allows the user to generate a fake triangle index for a new contact generated from merging of two other contacts. That index could later be used by the user to determine attributes of original triangles used as sources for a merged contact. The callback is currently used within OPCODE trimesh-sphere and OPCODE new trimesh-trimesh collisions.

If the callback is not assigned (the default) -1 is generated as triangle index for merged contacts.

NOTE: Before this API was introduced, the index was always set to the first triangle index.
Heightfield Class

dHeightfield is a regular grid heightfield collider. It can be used for heightmap terrains, but also for deformable animated water surfaces.
Heightfield Data

The dHeightfieldData is a storage class, similar to the dTrimeshData class, that holds all geom properties and optionally height sample data.

dHeightfieldDataID dGeomHeightfieldDataCreate ();
void dGeomHeightfieldDataDestroy (dHeightfieldDataID d)

Allocate and destroy dHeightfieldDataID objects. You must call dGeomHeightfieldDataDestroy to destroy it after the geom has been removed. The dHeightfieldDataID value is used when specifying a data format type.
Building from existing data

There are four functions to easily build a heightfield from an array of height values of different data types. They all have the same parameters, except the data type of the pHeightData pointer is different:

void dGeomHeightfieldDataBuildByte   (dHeightfieldDataID d,
                                      const unsigned char *pHeightData,
                                      int bCopyHeightData,
                                      dReal width, dReal depth,
                                      int widthSamples, int depthSamples,
                                      dReal scale, dReal offset, dReal thickness, int bWrap); 
void dGeomHeightfieldDataBuildShort  (dHeightfieldDataID d,
                                      const short *pHeightData,
                                      int bCopyHeightData,
                                      dReal width, dReal depth,
                                      int widthSamples, int depthSamples,
                                      dReal scale, dReal offset, dReal thickness, int bWrap); 
void dGeomHeightfieldDataBuildSingle (dHeightfieldDataID d,
                                      const float *pHeightData,
                                      int bCopyHeightData,
                                      dReal width, dReal depth,
                                      int widthSamples, int depthSamples,
                                      dReal scale, dReal offset, dReal thickness, int bWrap); 
void dGeomHeightfieldDataBuildDouble (dHeightfieldDataID d,
                                      const double *pHeightData,
                                      int bCopyHeightData,
                                      dReal width, dReal depth,
                                      int widthSamples, int depthSamples,
                                      dReal scale, dReal offset, dReal thickness, int bWrap);

Loads heightfield sample data into the HeightfieldData structure. Before a dHeightfieldDataID can be used by a geom it must be configured to specify the format of the height data. These calls all take the same parameters as listed below; the only difference is the data type pointed to by pHeightData.

    pHeightData is a pointer to the height data;
    bCopyHeightData specifies whether the height data should be copied to a local store. If zero, the data is accessed by reference and so must persist throughout the lifetime of the heightfield;
    width, height are the world space heightfield dimensions on the geom's local X and Z axes;
    widthSamples, depthSamples specifies the number of vertices to sample along the width and depth of the heightfield. Naturally this value must be at least two or more;
    scale is the vertical sample height multiplier, a uniform scale applied to all raw height data;
    offset is the vertical sample offset, added to the scaled height data;
    thickness is the thickness of AABB which is added below the lowest point, to prevent objects from falling through very thin heightfields;
    bWrap is 0 if the heightfield should be finite, 1 if should tile infinitely.

Retrieving data from a callback

typedef dReal (*dHeightfieldGetHeight) (void *userdata, int x, int z); 
void dGeomHeightfieldDataBuildCallback (dHeightfieldDataID d,
                                        void *pUserData,
                                        dHeightfieldGetHeight *pCallback,
                                        dReal width, dReal depth,
                                        int widthSamples, int depthSamples,
                                        dReal scale, dReal offset, dReal thickness, int bWrap);

This call specifies that the heightfield data is computed by the user and it should use the given callback when determining the height of a given element of its shape. The callback function is called while the simulation runs, and returns the value of the heightmap ("y" value) at a given (x,z) position.

    pUserData is a pointer for arbitrary user-defined data to pass to the callback
    pCallback is a pointer to the callback function
    The other arguments are the same as the other dGeomHeightfieldDataBuild* functions.

Setting terrain bounds

void dGeomHeightfieldDataSetBounds (dHeightfieldDataID d, dReal min_height, dReal max_height)

Sets the minimum and maximum height sample bounds in sample space. ODE does not automatically detect the sample data minimum and maximum height bounds, this must be done manually (for added flexibility and allows the user to control the process).

The default vertical sample bounds are infinite, so when the sample bounds are known, call this function for improved performance. Also, if the geom (built from this data) is rotated, its AABB will end up with NaNs, and break the collision detection; so always set the heightfield data's bounds to finite values if you are going to rotate the geom.
Heightfield Geom

dGeomID dCreateHeightfield(dSpaceID space, dHeightfieldDataID data, int bPlaceable);

Uses the information in the given dHeightfieldDataID to construct a geom representing a heightfield in a collision space.

    dHeightfieldDataID is the heightfield data object
    bPlaceable defines whether this geom can be transformed in the world using the usual functions such as dGeomSetPosition and dGeomSetRotation. If the geom is not set as placeable, then it uses a fixed orientation where the global Y axis represents the 'height' of the heightfield.

void dGeomHeightfieldSetHeightfieldData(dGeomID g, dHeightfieldDataID Data);
dHeightfieldDataID dGeomHeightfieldGetHeightfieldData(dGeomID g);

Set and retrieve the heightfield data object of this geom.


Geometry Transform Class

The geom transform classes are deprecated. Use geom offsets instead.

dGeomID dCreateGeomTransform (dSpaceID space);
void dGeomTransformSetGeom (dGeomID g, dGeomID obj);
dGeomID dGeomTransformGetGeom (dGeomID g);
void dGeomTransformSetCleanup (dGeomID g, int mode);
int dGeomTransformGetCleanup (dGeomID g);
void dGeomTransformSetInfo (dGeomID g, int mode);
int dGeomTransformGetInfo (dGeomID g);

If your code uses geom transforms, update it to use geom offsets instead, as soon as possible. The geom transform functions will be removed from the next release.
User defined classes

ODE's geometry classes are implemented internally as C++ classes. If you want to define your own geometry classes you can do this in two ways:

    Use the C functions in this section. This has the advantage of providing a clean separation between your code and ODE.
    Add the classes directly to ODE's source code. This has the advantage that you can use C++ so the implementation will potentially be a bit cleaner. This is also the preferred method if your collision class is generally useful and you want to contribute it to the public source base.

What follows is the C API for user defined geometry classes.

Every user defined geometry class has a unique integer number. A new geometry class (call it 'X') must provide the following to ODE:

    Functions that will handle collision detection and contact generation between X and one or more other classes. These functions must be of type dColliderFn, which is defined as:

typedef int dColliderFn (dGeomID o1, dGeomID o2, int flags, dContactGeom *contact, int skip);

This has exactly the same interface as dCollide. Each function will handle a specific collision case, where o1 has type X and o2 has some other known type.

    A "selector" function, of type dGetColliderFnFn, which is defined as:

typedef dColliderFn * dGetColliderFnFn (int num);

This function takes a class number (num), and returns the collider function that can handle colliding X with class num. It should return 0 if X does not know how to collide with class num. Note that if classes X and Y are to collide, only one needs to provide a function to collide with the other.

This function is called infrequently - the return values are cached and reused.

    A function that will compute the axis aligned bounding box (AABB) of instances of this class. This function must be of type dGetAABBFn, which is defined as:

typedef void dGetAABBFn (dGeomID g, dReal aabb[6]);

This function is given g, which has type X, and returns the axis-aligned bounding box for g. The aabb array has elements (minx, maxx, miny, maxy, minz, maxz). If you don't want to compute tight bounds for the AABB, you can just supply a pointer to dInfiniteAABB, which returns +/- infinity in each direction.

    The number of bytes of "class data" that instances of this class need. For example a sphere stores its radius in the class data area, and a box stores its side lengths there.

The following things are optional for a geometry class:

    A function that will destroy the class data. Most classes will not need this function, but some will want to deallocate heap memory or release other resources. This function must be of type dGeomDtorFn, which is defined as:

typedef void dGeomDtorFn (dGeomID o);

The argument o has type X.

    A function that will test whether a given AABB intersects with an instance of X. This is used as an early-exit test in the space collision functions. This function must be of type dAABBTestFn, which is defined as:

typedef int dAABBTestFn (dGeomID o1, dGeomID o2, dReal aabb2[6]);

The argument o1 has type X. If this function is provided it is called by dSpaceCollide when o1 intersects geom o2, which has an AABB given by aabb2. It returns 1 if aabb2 intersects o1, or 0 if it does not.

This is useful, for example, for large terrains. Terrains typically have very large AABBs, which are not very useful to test intersections with other objects. This function can test another object's AABB against the terrain without going to the computational trouble of calling the specific collision function. This has an especially big savings when testing against GeomGroup objects.

Here are the functions used to manage custom classes:

int dCreateGeomClass (const dGeomClass *classptr);

Register a new geometry class, defined by classptr. The number of the new class is returned. The convention used in ODE is to assign the class number to a global variable with the name dXxxClass where Xxx is the class name (e.g. dSphereClass).

Here is the definition of the dGeomClass structure:

struct dGeomClass {
    int bytes; // bytes of custom data needed
    dGetColliderFnFn *collider; // collider function
    dGetAABBFn *aabb; // bounding box function
    dAABBTestFn *aabb_test; // aabb tester, can be 0 for none
    dGeomDtorFn *dtor; // destructor, can be 0 for none
};

void * dGeomGetClassData (dGeomID);

Given a geom, return a pointer to the class's custom data (this will be a block of the required number of bytes).

dGeomID dCreateGeom (int classnum);

void dClosestLineSegmentPoints (const dVector3 a1, const dVector3 a2,
                                const dVector3 b1, const dVector3 b2,
                                dVector3 cp1, dVector3 cp2);

Given two line segments A and B with endpoints a1-a2 and b1-b2, return the points on A and B that are closest to each other (in cp1 and cp2). In the case of parallel lines where there are multiple solutions, a solution involving the endpoint of at least one line will be returned. This will work correctly for zero length lines, e.g. if a1==a2 and/or b1==b2.

int dBoxTouchesBox (const dVector3 p1, const dMatrix3 R1, const dVector3 side1,
                    const dVector3 p2, const dMatrix3 R2, const dVector3 side2);

Given boxes (p1,R1,side1) and (p2,R2,side2), return 1 if they intersect or 0 if not. p is the center of the box, R is the rotation matrix for the box, and side is a vector of x/y/z side lengths.

void dInfiniteAABB (dGeomID geom, dReal aabb[6]);

This function can be used as the AABB-getting function in a geometry class, if you don't want to compute tight bounds for the AABB. It returns +/- infinity in each direction.
Implementation notes
Large Environments

Often the collision world will contain many objects that are part of the static environment, that are not associated with rigid bodies. ODE's collision detection is optimized to detect geoms that do not move and to precompute as much information as possible about these objects to save time. For example, bounding boxes and internal collision data structures are precomputed.
Using a Different Collision Library

Using ODE's collision detection is optional - an alternative collision library can be used as long as it can supply dContactGeom structures to initialize contact joints.

The dynamics core of ODE is mostly independent of the collision library that is used, except for four points:

    The dGeomID type must be defined, as each body can store a pointer to the first geometry object that it is associated with.

    The dGeomMoved() function must be defined, with the following prototype:

void dGeomMoved (dGeomID);

This function is called by the dynamics code whenever a body moves: it indicates that the geometry object associated with the body is now in a new position.

    The dGeomGetBodyNext() function must be defined, with the following prototype:

dGeomID dGeomGetBodyNext (dGeomID);

This function is called by the dynamics code to traverse the list of geoms that are associated with each body. Given a geom attached to a body, it returns the next geom attached to that body, or 0 if there are no more geoms.

    The dGeomSetBody() function must be defined, with the following prototype:

void dGeomSetBody (dGeomID, dBodyID);


-}
