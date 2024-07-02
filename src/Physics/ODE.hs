module Physics.ODE (initODE, closeODE, withODE, step, quickStep)
where

import Control.Exception
import Control.Monad.IO.Class
import Physics.ODE.Raw.Types as World
import Physics.ODE.Raw.World as World
import Physics.ODE.World (create)

-----------------------------------------------------------
initODE :: IO ()
{-# INLINE initODE #-}
initODE =
    World.c'initODE

-----------------------------------------------------------
closeODE :: IO ()
{-# INLINE closeODE #-}
closeODE =
    World.c'closeODE

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
step :: World -> DeltaTime Float -> IO ()
{-# INLINE step #-}
step =
    c'stepdWorldStep

-----------------------------------------------------------
quickStep :: World -> DeltaTime Float -> IO ()
{-# INLINE quickStep #-}
quickStep =
    c'quickStepdWorldQuickStep
