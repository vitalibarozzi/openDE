{-# LANGUAGE EmptyDataDecls #-}

module Physics.ODE.Raw.Types where

import Foreign

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

--typedef struct dMass {
--    dReal mass; // total mass of the rigid body
--    dVector3 c; // center of gravity position in body frame (x,y,z)
--    dMatrix3 I; // 3x3 inertia tensor in body frame, about POR
--} dMass;
data MassValue = MassValue

type DeltaTime a = a
type Matrix3 = Ptr Float
type Quaternion = (Float, Float, Float, Float) -- TODO make v4
type RawCallback = Ptr () -> Ptr GeomStruct -> Ptr GeomStruct -> IO ()


data ContactGeom = ContactGeom
    { contactPos     :: (Float, Float, Float) -- TODO make V3
    , contactNormal  :: (Float, Float, Float) -- TODO make V3
    , contactDepth   :: Float
    , contactObjects :: (Geom, Geom) -- TODO make v2
    }
    deriving (Show)

data ContactInfo = ContactInfo
    { contactSurface :: Surface
    , contactGeom    :: ContactGeom
    , contactFDir1   :: (Float, Float, Float) -- TODO make v3
    }
    deriving (Show)

data Surface = Surface
    {- TODO make it like this
     -
     int mode;

     OK dReal mu; 
     OK dReal mu2;

     dReal rho;
     dReal rho2;
     dReal rhoN;

     OK dReal bounce;
     OK dReal bounce_vel;

     OK dReal soft_erp;
     OK dReal soft_cfm;

     OK dReal motion1, motion2, motionN;

     OK dReal slip1, slip2;
};
-}
    { surfdaceMode :: [SurfaceMode]
    , surfaceMu :: Float
    , surfaceMu2 :: Maybe Float
    -- surfaceRho1
    -- surfaceRho2
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
    -- TODO missing a couple?
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
