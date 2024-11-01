-- | Execution.
module Physics.ODE (
    withODE,
    initODE,
    closeODE,
    step,
    quickStep,
)
where

import Control.Exception
import Control.Monad.IO.Class
import Physics.ODE.Raw.Types as World
import Physics.ODE.Raw.World as World
import Physics.ODE.World (create)


-----------------------------------------------------------
{- | Wrapper around initODE+closeODE with `bracket` and a
convenience `World` created.
-}
withODE :: (MonadIO m) => (World.World -> IO ()) -> m ()
{-# INLINEABLE withODE #-}
withODE k =
    liftIO
        $ bracket
            initODE
            (\() -> closeODE)
            (\() -> create >>= k)


-----------------------------------------------------------
initODE :: (MonadIO m) => m ()
{-# INLINE initODE #-}
initODE =
    liftIO World.c'initODE


-----------------------------------------------------------
closeODE :: (MonadIO m) => m ()
{-# INLINE closeODE #-}
closeODE =
    liftIO World.c'closeODE


-----------------------------------------------------------
step :: (MonadIO m) => World -> DeltaTime Float -> m ()
{-# INLINE step #-}
step w d =
    liftIO (c'stepdWorldStep w d)


-----------------------------------------------------------
quickStep :: (MonadIO m) => World -> DeltaTime Float -> m ()
{-# INLINE quickStep #-}
quickStep w d =
    liftIO (c'quickStepdWorldQuickStep w d)
