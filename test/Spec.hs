
import Physics.ODE.Body as Body
import Physics.ODE.Raw.World as World
import Physics.ODE.World as World
import Physics.ODE.Raw.Joint as Joint
import Physics.ODE.Raw.Objects as Object
import Physics.ODE.Geom as Geom
import Data.StateVar


--foo = 
--    Ball
--        (Sphere 10)
--        (Sphere 32)


main :: IO ()
main = do
    World.withODE $ \w -> do

        b0 <- Body.create w
        b1 <- Body.create w

        m0 <- Object.createSphere Nothing 10
        m1 <- Object.createSphere Nothing 32

        j0 <- Joint.createBall w Nothing

        __ <- Geom.setBody m0 (Just b0)
        __ <- Geom.setBody m1 (Just b1)
        __ <- Joint.attach j0 (Just b0) (Just b1)

        x <- get (Body.position b0)
        y <- get (Body.position b1)
        print (x,y)
        World.step w 0.1
        Body.position b0 $= (10, 10, 10)
        World.step w 0.1
        x <- get (Body.position b0)
        y <- get (Body.position b1)
        print (x,y)
        putStrLn "Test suite not yet implemented"
