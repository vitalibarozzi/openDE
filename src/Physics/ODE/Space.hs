module Physics.ODE.Space
    ( createSimple
    , createHash
    , createQuadTree
    , destroySpace
    , level
    , cleanup
    , sublevel
    , add
    , remove
    , query
    , getNumGeoms
    , getGeom
    , module Physics.ODE.Raw.Space
    )
where

import Data.Maybe
import Foreign
import Physics.ODE.Raw.Space
import Physics.ODE.Raw.Types
import Control.Monad.IO.Class
import Data.StateVar


-----------------------------------------------------------
{-# INLINE createSimple #-}
createSimple :: (MonadIO m) => Maybe Space -> m Space
createSimple mspace =
    liftIO $ do
        c'dSimpleSpaceCreate $ case mspace of
            Data.Maybe.Just a_1 -> a_1
            Data.Maybe.Nothing  -> nullPtr
        

-----------------------------------------------------------
{-# INLINE createHash #-}
createHash :: (MonadIO m) => Maybe Space -> m Space
createHash arg_0 =
    liftIO $ do
        c'dHashSpaceCreate $ case arg_0 of
            Data.Maybe.Just a_1 -> a_1
            Data.Maybe.Nothing  -> nullPtr


-----------------------------------------------------------
{-# INLINE createQuadTree #-}
createQuadTree :: (MonadIO m) => Maybe Space -> Int -> m Space
createQuadTree s arg_1 = do
    ptr1 <- undefined -- TODO
    ptr2 <- undefined -- TODO
    case s of
        Nothing -> liftIO $ c'dQuadTreeSpaceCreate nullPtr ptr1 ptr2 arg_1
        Just ss -> liftIO $ c'dQuadTreeSpaceCreate ss ptr1 ptr2 arg_1


-----------------------------------------------------------
{-# INLINE destroySpace #-}
destroySpace :: (MonadIO m) => Space -> m ()
destroySpace =
    liftIO . c'dSpaceDestroy


-----------------------------------------------------------
{-# INLINE level #-}
level :: Space -> StateVar (Int,Int)
level s =
    StateVar _get _set
  where
     _get = 
         alloca $ \m3 -> 
         alloca $ \m4 -> do
                c'dHashSpaceGetLevels s m3 m4
                r1 <- peek m3
                r2 <- peek m4
                return (r2, r2)
     _set (a,b) = 
         c'dHashSpaceSetLevels s a b


-----------------------------------------------------------
{-# INLINE sublevel #-}
sublevel :: Space -> StateVar Int
sublevel s = do
    StateVar _get _set
  where
    _get = c'dSpaceGetSublevel s
    _set = c'dSpaceSetSublevel s


-----------------------------------------------------------
{-# INLINE cleanup #-}
cleanup :: Space -> StateVar Int
cleanup s = do
    StateVar _get _set
  where
    _get = c'dSpaceGetCleanup s
    _set = c'dSpaceSetCleanup s
   

-----------------------------------------------------------
{-# INLINE add #-}
add :: (MonadIO m) => Space -> Geom -> m ()
add s arg_1 =
    liftIO $ c'dSpaceAdd s arg_1


-----------------------------------------------------------
{-# INLINE remove #-}
remove :: (MonadIO m) => Space -> Geom -> m ()
remove s arg_1 =
    liftIO $ c'dSpaceRemove s arg_1


-----------------------------------------------------------
{-# INLINE query #-}
query :: (MonadIO m) => Space -> Geom -> m Int
query s arg_1 =
    liftIO $ c'dSpaceQuery s arg_1


-----------------------------------------------------------
{-# INLINE getNumGeoms #-}
getNumGeoms :: (MonadIO m) => Space -> m Int
getNumGeoms s =
    liftIO $ c'dSpaceGetNumGeoms s


-----------------------------------------------------------
{-# INLINE getGeom #-}
getGeom :: (MonadIO m) => Space -> Int -> m Geom
getGeom s arg_1 =
    liftIO $ c'dSpaceGetGeom s arg_1
