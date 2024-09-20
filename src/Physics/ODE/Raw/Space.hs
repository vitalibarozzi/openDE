module Physics.ODE.Raw.Space
 ( c'dSimpleSpaceCreate
 , c'dHashSpaceCreate
 , c'dSpaceDestroy
 , c'dHashSpaceSetLevels
 , c'dHashSpaceGetLevels
 , c'dSpaceSetCleanup
 , c'dSpaceAdd
 , c'dSpaceGetNumGeoms
 , c'dSpaceSetSublevel
 , c'dSpaceGetSublevel
 , c'adddSpaceAdd
 , c'dSpaceRemove
 , c'dSpaceQuery
 , c'dSpaceGetGeom
 , c'dQuadTreeSpaceCreate
 )
where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dSimpleSpaceCreate" {----------} c'dSimpleSpaceCreate{-----}   :: Space -> IO Space
foreign import ccall unsafe "dHashSpaceCreate" {------------} c'dHashSpaceCreate {------}   :: Space -> IO Space
foreign import ccall unsafe "dSpaceDestroy" {---------------} c'dSpaceDestroy {---------}   :: Space -> IO ()
foreign import ccall unsafe "dHashSpaceSetLevels" {---------} c'dHashSpaceSetLevels {---}   :: Space -> Int -> Int -> IO ()
foreign import ccall unsafe "dHashSpaceGetLevels" {---------} c'dHashSpaceGetLevels {---}   :: Space -> Ptr Int -> Ptr Int -> IO ()
foreign import ccall unsafe "dSpaceSetCleanup" {------------} c'dSpaceSetCleanup {------}   :: Space -> Int -> IO ()
foreign import ccall unsafe "dSpaceGetCleanup" {------------} c'dSpaceAdd {-------------}   :: Space -> IO Int
foreign import ccall unsafe "dSpaceGetNumGeoms" {-----------} c'dSpaceGetNumGeoms {-----}   :: Space -> IO Int
foreign import ccall unsafe "dSpaceSetSublevel" {-----------} c'dSpaceSetSublevel {-----}   :: Space -> Int -> IO ()
foreign import ccall unsafe "dSpaceGetSublevel" {-----------} c'dSpaceGetSublevel {-----}   :: Space -> IO Int
foreign import ccall unsafe "dSpaceAdd" {-------------------} c'adddSpaceAdd {----------}   :: Space -> Geom -> IO () -- TODO is this type correct?
foreign import ccall unsafe "dSpaceRemove" {----------------} c'dSpaceRemove {----------}   :: Space -> Geom -> IO () -- TODO is this type correct?
foreign import ccall unsafe "dSpaceQuery" {-----------------} c'dSpaceQuery {-----------}   :: Space -> Geom -> IO Int -- TODO is this type correct?
foreign import ccall unsafe "dSpaceGetGeom" {---------------} c'dSpaceGetGeom {---------}   :: Space -> Int -> IO Geom -- TODO is this type correct?
foreign import ccall unsafe "dQuadTreeSpaceCreate" {--------} c'dQuadTreeSpaceCreate {--}   :: Space -> Int -> IO Geom-- TODO is this type correct?

{-
dSpaceID dQuadTreeSpaceCreate (dSpaceID space, dVector3 Center, dVector3 Extents, int Depth);
Creates a quadtree space. center and extents define the size of the root block. depth sets the depth of the tree - the number of blocks that are created is 4^depth.
void dSpaceSetSublevel (dSpaceID space, int sublevel);
int dSpaceGetSublevel (dSpaceID space);
-}
