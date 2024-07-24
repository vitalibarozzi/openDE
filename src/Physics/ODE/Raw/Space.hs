module Physics.ODE.Raw.Space
where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dSimpleSpaceCreate" c'createSimpledSimpleSpaceCreate :: Space -> IO Space
foreign import ccall unsafe "dHashSpaceCreate" c'createHashdHashSpaceCreate       :: Space -> IO Space
foreign import ccall unsafe "dSpaceDestroy" c'destroySpacedSpaceDestroy           :: Space -> IO ()
foreign import ccall unsafe "dHashSpaceSetLevels" c'setLevelsdHashSpaceSetLevels  :: Space -> Int -> Int -> IO ()
foreign import ccall unsafe "dHashSpaceGetLevels" c'getLevelsdHashSpaceGetLevels  :: Space -> Ptr Int -> Ptr Int -> IO ()
foreign import ccall unsafe "dSpaceSetCleanup" c'setCleanupdSpaceSetCleanup       :: Space -> Int -> IO ()
foreign import ccall unsafe "dSpaceGetCleanup" c'getCleanupdSpaceGetCleanup       :: Space -> IO Int
foreign import ccall unsafe "dSpaceAdd" c'adddSpaceAdd                            :: Space -> Geom -> IO ()
foreign import ccall unsafe "dSpaceRemove" c'removedSpaceRemove                   :: Space -> Geom -> IO ()
foreign import ccall unsafe "dSpaceQuery" c'querydSpaceQuery                      :: Space -> Geom -> IO Int
foreign import ccall unsafe "dSpaceGetNumGeoms" c'getNumGeomsdSpaceGetNumGeoms    :: Space -> IO Int
foreign import ccall unsafe "dSpaceGetGeom" c'getGeomUnsafedSpaceGetGeom          :: Space -> Int -> IO Geom

{-
dSpaceID dSimpleSpaceCreate (dSpaceID space);
dSpaceID dHashSpaceCreate (dSpaceID space);

Create a space, either of the simple or multi-resolution hash table kind. If space is nonzero, insert the new space into that space.

dSpaceID dQuadTreeSpaceCreate (dSpaceID space, dVector3 Center, dVector3 Extents, int Depth);

Creates a quadtree space. center and extents define the size of the root block. depth sets the depth of the tree - the number of blocks that are created is 4^depth.

void dSpaceDestroy (dSpaceID);

This destroys a space. It functions exactly like dGeomDestroy except that it takes a dSpaceID argument. When a space is destroyed, if its cleanup mode is 1 (the default) then all the geoms in that space are automatically destroyed as well.

void dHashSpaceSetLevels (dSpaceID space, int minlevel, int maxlevel);
void dHashSpaceGetLevels (dSpaceID space, int *minlevel, int *maxlevel);

Sets and get some parameters for a multi-resolution hash table space. The smallest and largest cell sizes used in the hash table will be 2^minlevel and 2^maxlevel respectively. minlevel must be less than or equal to maxlevel.

In dHashSpaceGetLevels the minimum and maximum levels are returned through pointers. If a pointer is zero then it is ignored and no argument is returned.

void dSpaceSetCleanup (dSpaceID space, int mode);
int dSpaceGetCleanup (dSpaceID space);

Set and get the clean-up mode of the space. If the clean-up mode is 1, then the contained geoms will be destroyed when the space is destroyed. If the clean-up mode is 0 this does not happen. The default clean-up mode for new spaces is 1.

void dSpaceSetSublevel (dSpaceID space, int sublevel);
int dSpaceGetSublevel (dSpaceID space);

Set and get sublevel value for the space. Sublevel affects how the space is handled in dSpaceCollide2 when it is collided with another space. If sublevels of both spaces match, the function iterates geometries of both spaces and collides them with each other. If sublevel of one space is greater than the sublevel of another one, only the geometries of the space with greater sublevel are iterated, another space is passed into collision callback as a geometry itself. By default all the spaces are assigned zero sublevel.

Note! The space sublevel IS NOT automatically updated when one space is inserted into another or removed from one. It is a client's responsibility to update sublevel value if necessary.

void dSpaceAdd (dSpaceID, dGeomID);

Add a geom to a space. This function can be called automatically if a space argument is given to a geom creation function.

void dSpaceRemove (dSpaceID, dGeomID);

Remove a geom from a space. This does nothing if the geom is not actually in the space. This function is called automatically by dGeomDestroy if the geom is in a space.

int dSpaceQuery (dSpaceID, dGeomID);

Return 1 if the given geom is in the given space, or return 0 if it is not.

int dSpaceGetNumGeoms (dSpaceID);

Return the number of geoms contained within a space.

dGeomID dSpaceGetGeom (dSpaceID, int i);

-}
