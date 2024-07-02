{-# LANGUAGE EmptyDataDecls #-}

module Physics.ODE.Raw.Types where

import Foreign

type DeltaTime a = a

type Matrix3 = Ptr Float

type Quaternion = (Float, Float, Float, Float) -- TODO make v4

type RawCallback = Ptr () -> Ptr GeomStruct -> Ptr GeomStruct -> IO ()

data WorldStruct
data SpaceStruct
data BodyStruct
data GeomStruct
data JointStruct
data JointGroupStruct
data MassStruct

type Mass = ForeignPtr MassStruct -- whys is this one different?
type World = Ptr WorldStruct
type Space = Ptr SpaceStruct
type Body = Ptr BodyStruct
type Geom = Ptr GeomStruct
type Joint = Ptr JointStruct
type JointGroup = Ptr JointGroupStruct

data ContactGeom = ContactGeom
    { contactPos :: (Float, Float, Float) -- TODO make V3
    , contactNormal :: (Float, Float, Float) -- TODO make V3
    , contactDepth :: Float
    , contactObjects :: (Geom, Geom) -- TODO make v2
    }
    deriving (Show)

data ContactInfo = ContactInfo
    { contactSurface :: Surface
    , contactGeom :: ContactGeom
    , contactFDir1 :: (Float, Float, Float) -- TODO make v3
    }
    deriving (Show)

data Surface = Surface
    { surfaceMu :: Float
    , surfaceMu2 :: Maybe Float
    , surfaceBounce :: Maybe (Float, Float)
    , surfaceSoftERP :: Maybe Float
    , surfaceSoftCFM :: Maybe Float
    , surfaceMotion1 :: Maybe Float
    , surfaceMotion2 :: Maybe Float
    , surfaceSlip1 :: Maybe Float
    , surfaceSlip2 :: Maybe Float
    }
    deriving (Show, Eq)

data SurfaceMode
    = HaveMu2
    | HaveFDir1
    | HaveBounce
    | HaveSoftERP
    | HaveSoftCFM
    | HaveMotion1
    | HaveMotion2
    | HaveSlip1
    | HaveSlip2
    | HaveApprox11
    | HaveApprox12
    deriving (Show, Eq, Enum, Bounded)

data JointType
    = Ball
    | Hinge
    | Slider
    | Contact
    | Universal
    | Hinge2
    | Fixed
    | AMotor
    deriving (Show, Eq, Ord)

data BodyIndex
    = First
    | Second
    deriving (Show, Eq, Ord)

data GeomClass
    = Sphere
    | Box
    | CappedCylinder
    | Cylinder
    | Plane
    | GeomTransform
    | Ray
    | TriangleMesh
    | SimpleSpace
    | HashSpace
    deriving (Show, Eq, Ord)

data RotationMode
    = Infinitesimal
    | Finite Float Float Float
    deriving (Show, Eq, Ord)
