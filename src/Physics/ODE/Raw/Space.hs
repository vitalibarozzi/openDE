module Physics.ODE.Raw.Space
 ( c'dSimpleSpaceCreate
 , c'dHashSpaceCreate
 , c'dSpaceDestroy
 , c'dHashSpaceSetLevels
 , c'dHashSpaceGetLevels
 , c'dSpaceSetCleanup
 , c'dSpaceGetCleanup
 , c'dSpaceAdd
 , c'dSpaceGetNumGeoms
 , c'dSpaceSetSublevel
 , c'dSpaceGetSublevel
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
foreign import ccall unsafe "dSpaceGetCleanup" {------------} c'dSpaceGetCleanup {-------------}   :: Space -> IO Int
foreign import ccall unsafe "dSpaceGetNumGeoms" {-----------} c'dSpaceGetNumGeoms {-----}   :: Space -> IO Int
foreign import ccall unsafe "dSpaceSetSublevel" {-----------} c'dSpaceSetSublevel {-----}   :: Space -> Int -> IO ()
foreign import ccall unsafe "dSpaceGetSublevel" {-----------} c'dSpaceGetSublevel {-----}   :: Space -> IO Int
foreign import ccall unsafe "dSpaceAdd" {-------------------} c'dSpaceAdd {----------}   :: Space -> Geom -> IO ()
foreign import ccall unsafe "dSpaceRemove" {----------------} c'dSpaceRemove {----------}   :: Space -> Geom -> IO ()
foreign import ccall unsafe "dSpaceQuery" {-----------------} c'dSpaceQuery {-----------}   :: Space -> Geom -> IO Int
foreign import ccall unsafe "dSpaceGetGeom" {---------------} c'dSpaceGetGeom {---------}   :: Space -> Int -> IO Geom -- TODO is this type correct?
foreign import ccall unsafe "dQuadTreeSpaceCreate" {--------} c'dQuadTreeSpaceCreate {--}   :: Space -> Ptr Float -> Ptr Float -> Int -> IO Space-- TODO is this type correct?
--dSpaceID dQuadTreeSpaceCreate (dSpaceID space, dVector3 Center, dVector3 Extents, int Depth);
