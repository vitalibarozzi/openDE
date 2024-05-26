module Physics.ODE.World where

import Data.Maybe
import Data.Typeable
import Foreign
import Physics.ODE.Raw.World as World
import Physics.ODE.Raw.Types as World
import Control.Exception
import Control.Monad.IO.Class



--createObj :: (MonadIO m) => Object -> m ()
--createObj = \case
    --Sphere Double
    --Box    ()
    --Plane  ()
    --Ball o0 o1 -> do
    --    oo0 <- createObjPrim o0
    --    oo1 <- createObjPrim o1
    --Hinge  Object Object
    --Slider Object Object


--data Object
--    = Sphere Double
--    | Box    ()
--    | Plane  ()
--    | Ball   Object Object
--    | Hinge  Object Object
--    | Slider Object Object



withODE :: (MonadIO m) => (World.World -> IO ()) -> m ()
withODE k = 
    liftIO $ bracket
        World.initODE
        (\() ->  World.closeODE)
        (\() -> do
            w <- World.create 
            k w
        )
