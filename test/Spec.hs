{-# LANGUAGE BlockArguments #-}
import Physics.ODE.Body as Body
import Physics.ODE.World as World
import Physics.ODE as ODE
import Physics.ODE.Collision as Collision
import Physics.ODE.Joint as Joint
import Physics.ODE.Objects as Objects
import Physics.ODE.Geom as Geom
import Physics.ODE.Mass as Mass
import Physics.ODE.Raw.Types
import Data.StateVar
import Test.Hspec
import Control.Monad


main :: IO ()
main = do
    ODE.withODE $ \w -> do
        b0 <- Body.create w
        b1 <- Body.create w

        m0 <- Objects.createSphere Nothing 10
        m1 <- Objects.createSphere Nothing 32

        j0 <- Joint.createBall w Nothing

        __ <- Geom.setBody m0 (Just b0)
        __ <- Geom.setBody m1 (Just b1)
        __ <- Joint.attach j0 (Just b0) (Just b1)

        x <- get (Body.position b0)
        y <- get (Body.position b1)
        print (x,y)
        ODE.step w 0.1
        Body.position b0 $= (10, 10, 10)
        ODE.step w 0.1
        x_ <- get (Body.position b0)
        y_ <- get (Body.position b1)
        print (x_,y_)
        hspec do
            testMass
            testBody
            testCollision
            testGeom
            testJoint
            testObjects
            testRotation
            testSpace

testMass :: Spec
testMass = do
    context "Mass.hs" do
        ww <- runIO $ World.create
        m0 <- runIO $ Mass.create
        __ <- runIO $ ODE.step ww 1
        v0 <- runIO $ get (Mass.mass m0)
        it "start at 0" (v0 == 0)
        __ <- runIO $ Mass.mass m0 $= 0.12
        v1 <- runIO $ get (Mass.mass m0)
        it "can be adjusted to 1" (v1 == 0.12)
        runIO $ Mass.setZero m0
        v2 <- runIO $ get (Mass.mass m0)
        it "can be reset to 0 using setZero" (v2 == 0)
        b0 <- runIO $ Body.create ww
        m1 <- runIO $ get (Body.mass b0)
        v3 <- runIO $ get (Mass.mass m1)
        it "body mass starts at 1" (v3 == 1.0)
        __ <- runIO $ Mass.mass m1 $= 1.1
        v4 <- runIO $ get (Mass.mass m1)
        it "body mass can be adjusted" (v4 == 1.1)
        runIO $ Mass.setZero m1
        v5 <- runIO $ get (Mass.mass m1)
        it "body mass can be set to 0" (v5 == 0.0)

testCollision :: Spec
testCollision = do
    context "Collision.hs" do 
        runIO do
            ww <- World.create
            World.gravity ww $= (0, -9.81, 0)
            bg0 <- Objects.createBox Nothing 1 1 1 
            bg1 <- Objects.createBox Nothing 1 1 1 
            b0 <- Body.create ww
            b1 <- Body.create ww
            Geom.setBody bg0 (Just b0)
            Geom.setBody bg1 (Just b1)
            Body.position b0 $= (0,0,0)
            Body.position b1 $= (0,1.2,0)
            Body.gravityMode b0 $= False
            print =<< get (Body.position b1)
            ODE.step ww 0.167
            print =<< get (Body.position b1)
            cgroup <- Joint.createGroup
            collisions <- Collision.collide bg1 bg0 10
            ys <- forM collisions \con -> do
                        c <- Joint.createContact ww (Just cgroup) (con 
                                           { contactGeom = (contactGeom con) 
                                                   { contactObjects = (bg0,bg1) 
                                                   }
                                           , contactSurface = (contactSurface con)
                                                   { surfaceMu = 0
                                                   , surfaceBounce = Just (0.9,0.1)
                                                   , surfaceSoftCFM = Just 0.00001
                                                   }
                                           })
                        Joint.attach c (Just b1) Nothing -- (Just b1)
                        --error . show $ con
                        pure c
            ODE.step ww 0.170
            print =<< get (Body.position b1)
            Joint.destroyGroup cgroup
            forM_ ys \j -> do
                Joint.destroyJoint j
            ODE.step ww 0.170
            print =<< get (Body.position b1)
            World.setCFM ww 0.1
            print =<< World.getCFM ww
            --collisions <- Collision.collide bg0 bg1 1
            --forM_ collisions \col -> do
            --    print col
        describe "" do 
            it "" pending


testBody :: Spec
testBody = context "Body.hs" do describe "" do it "" pending


testGeom :: Spec
testGeom = context "Geom.hs" do describe "" do it "" pending

testJoint :: Spec
testJoint = context "Joint.hs" do describe "" do it "" pending
                        --Joint.attach c (Just b0) (Just b1)
                        --Joint.jointDisable c
                        --print =<< Joint.jointIsEnabled c

testObjects :: Spec
testObjects = context "Objects.hs" do describe "" do it "" pending

testRotation :: Spec
testRotation = context "Rotation.hs" do describe "" do it "" pending

testSpace :: Spec
testSpace = context "Space.hs" do describe "" do it "" pending
