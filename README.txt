OpenDE
------
A physics engine package for Haskell. Forked from 
https://hackage.haskell.org/package/HODE, then started cleaning 
it and adding StateVar API to match both OpenGL and OpenAL, 
adding MonadIO, and whatever else was missing.



About ODE
---------
"ODE is an open source, high performance library for simulating 
rigid body dynamics. It has advanced joint types and integrated 
collision detection with friction. ODE is useful for simulating 
vehicles, objects in virtual reality environments and virtual 
creatures. It is currently used in many computer games, 3D 
authoring tools and simulation tools. See http://www.ode.org/"



Completion
----------
[100%] Objects
[ 80%] Space
[ 70%] Body
[ 70%] Collision
[ 70%] Rotation
[ 70%] World
[ 50%] Mass
[ 10%] Tests
[  1%] Error
[ -- ] Geom
[ -- ] Joint



Example usage
-------------
```
import qualified Physics.ODE.World    as World
import qualified Physics.ODE          as ODE
import qualified Physics.ODE.Geom     as ODE
import qualified Physics.ODE.Body     as Body
import           Physics.ODE.StateVar (($=!),get)
import           Physics.ODE.Linear   (V3)
import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
    let delay {- in milliseconds -} = 100000                       :: Int
    let dt    {- in seconds      -} = realToFrac delay / 10000000  :: Double
    let g     {- in m/s          -} = -9.81                        :: Double
    let h     {- in meters       -} = 20.0                         :: Double
    putStrLn "Starting ODE example usage."
    () <- ODE.initODE
    wd <- World.create
    sp <- Space.create wd
    bg <- Object.createSphere (Just sp) 1
    mb <- Geometry.getBody bg
    case mb of
        Nothiing -> error "No body found."
        Just bb -> do
            () <- World.gravity wd ODE.$=! -9.81
            () <- Body.position bb ODE.$=! V3 0 h 0
            Control.Monad.forM_ [1..10] $ \n -> do
                ________ <- Control.Concurrent.threadDelay delay
                ________ <- ODE.step dt wd
                V3 _ y _ <- ODE.get (Body.position bb) 
                Prelude.putStrLn ("Ball position: "<>Prelude.show y<>"m")
    () <- ODE.closeODE
    putStrLn "ODE example is done."
```
