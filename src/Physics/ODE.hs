-- | Execution.
module Physics.ODE (
    initODE,
    closeODE,
    withODE,
    withODE_,
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

{- | Wrapper around initODE+closeODE with `bracket` and a
convinience `World` created, although other worlds may be
created subsequently if needed.
-}
withODE :: (MonadIO m) => (World.World -> IO ()) -> m ()
{-# INLINEABLE withODE #-}
withODE k =
    liftIO
        ( bracket
            initODE
            (\() -> closeODE)
            (\() -> k =<< create)
        )

-----------------------------------------------------------
-- wtf?
withODE_ :: (MonadIO m) => (World.World -> m ()) -> m ()
{-# INLINEABLE withODE_ #-}
withODE_ k = do
    liftIO initODE
    k =<< liftIO create
    liftIO closeODE

-- TODO add comments about the difference between the 2

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
