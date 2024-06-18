module Physics.ODE.Raw.Space
where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dSimpleSpaceCreate" c'createSimpledSimpleSpaceCreate :: Space -> IO Space
foreign import ccall unsafe "dHashSpaceCreate" c'createHashdHashSpaceCreate :: Space -> IO Space
foreign import ccall unsafe "dSpaceDestroy" c'destroySpacedSpaceDestroy :: Space -> IO ()
foreign import ccall unsafe "dHashSpaceSetLevels" c'setLevelsdHashSpaceSetLevels :: Space -> Int -> Int -> IO ()
foreign import ccall unsafe "dHashSpaceGetLevels" c'getLevelsdHashSpaceGetLevels :: Space -> Ptr Int -> Ptr Int -> IO ()
foreign import ccall unsafe "dSpaceSetCleanup" c'setCleanupdSpaceSetCleanup :: Space -> Int -> IO ()
foreign import ccall unsafe "dSpaceGetCleanup" c'getCleanupdSpaceGetCleanup :: Space -> IO Int
foreign import ccall unsafe "dSpaceAdd" c'adddSpaceAdd :: Space -> Geom -> IO ()
foreign import ccall unsafe "dSpaceRemove" c'removedSpaceRemove :: Space -> Geom -> IO ()
foreign import ccall unsafe "dSpaceQuery" c'querydSpaceQuery :: Space -> Geom -> IO Int
foreign import ccall unsafe "dSpaceGetNumGeoms" c'getNumGeomsdSpaceGetNumGeoms :: Space -> IO Int
foreign import ccall unsafe "dSpaceGetGeom" c'getGeomUnsafedSpaceGetGeom :: Space -> Int -> IO Geom
