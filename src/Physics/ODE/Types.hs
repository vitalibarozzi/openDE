{-# LINE 1 "src/Physics/ODE/Types.hsc" #-}
{-# LANGUAGE EmptyDataDecls #-}

module Physics.ODE.Types where

import Foreign

type ODEreal = Double
{-# LINE 8 "src/Physics/ODE/Types.hsc" #-}

type Matrix3 = Ptr ODEreal
type Quaternion = (ODEreal, ODEreal, ODEreal, ODEreal)

data WorldStruct
type World = Ptr WorldStruct

data SpaceStruct
type Space = Ptr SpaceStruct

data BodyStruct
type Body = Ptr BodyStruct

data GeomStruct
type Geom = Ptr GeomStruct

data JointStruct
type Joint = Ptr JointStruct

data JointGroupStruct
type JointGroup = Ptr JointGroupStruct

data MassStruct
type Mass = ForeignPtr MassStruct

data ContactGeom
    = ContactGeom
    { contactPos    :: (ODEreal, ODEreal, ODEreal)
    , contactNormal :: (ODEreal, ODEreal, ODEreal)
    , contactDepth  :: ODEreal
    , contactObjects:: (Geom,Geom)
    } deriving (Show)

data ContactInfo
    = ContactInfo
    { contactSurface :: Surface
    , contactGeom    :: ContactGeom
    , contactFDir1   :: (ODEreal,ODEreal,ODEreal)
    } deriving (Show)

data Surface
    = Surface
    { surfaceMu      :: ODEreal
    , surfaceMu2     :: Maybe ODEreal
    , surfaceBounce  :: Maybe (ODEreal,ODEreal)
    , surfaceSoftERP :: Maybe ODEreal
    , surfaceSoftCFM :: Maybe ODEreal
    , surfaceMotion1 :: Maybe ODEreal
    , surfaceMotion2 :: Maybe ODEreal
    , surfaceSlip1   :: Maybe ODEreal
    , surfaceSlip2   :: Maybe ODEreal
    } deriving (Show,Eq)



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
      deriving (Show,Eq,Enum,Bounded)

data JointType
    = Ball
    | Hinge
    | Slider
    | Contact
    | Universal
    | Hinge2
    | Fixed
    | AMotor
      deriving (Show,Eq,Ord)

data BodyIndex
    = First
    | Second
      deriving (Show,Eq,Ord)

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
      deriving (Show,Eq,Ord)

data RotationMode
    = Infinitesimal
    | Finite ODEreal ODEreal ODEreal
      deriving (Show,Eq,Ord)
