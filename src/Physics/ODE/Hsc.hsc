#include <ode/ode.h>
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Physics.ODE.Hsc
    ( addressOfGeom
    , toBitmask
    , fromBitmask
    , sizeOfMass
    , sizeOfMatrix3
    , sizeOfMatrix4
    , peekMass
    , toSurfaceMode
    , fromSurfaceMode
    , toJointType
    , fromJointType
    , toBodyIndex
    , fromBodyIndex
    , toGeomClass
    , fromGeomClass
    ) where

import Physics.ODE.Types
import Physics.ODE.Utilities
import Foreign

import Control.Monad
import Data.Maybe

instance Storable ContactGeom where
    sizeOf _ = #{size struct dContactGeom}
    alignment _ = 4
    peek ptr
        = do pos    <- peekVector3 (#{ptr struct dContactGeom, pos} ptr)
             normal <- peekVector3 (#{ptr struct dContactGeom, normal} ptr)
             depth  <- peek (#{ptr struct dContactGeom, depth} ptr)
             obj1   <- peek (#{ptr struct dContactGeom, g1} ptr)
             obj2   <- peek (#{ptr struct dContactGeom, g2} ptr)
             return (ContactGeom pos normal depth (obj1,obj2))
    poke ptr cGeom
        = do pokeVector3 (#{ptr struct dContactGeom, pos} ptr) (contactPos cGeom)
             pokeVector3 (#{ptr struct dContactGeom, normal} ptr) (contactNormal cGeom)
             #{poke struct dContactGeom, depth} ptr (contactDepth cGeom)
             #{poke struct dContactGeom, g1} ptr (fst (contactObjects cGeom))
             #{poke struct dContactGeom, g2} ptr (snd (contactObjects cGeom))

instance Storable ContactInfo where
    sizeOf _ = #{size struct dContact}
    alignment _ = alignment (undefined :: ODEreal)
    peek ptr
        = do surface <- #{peek struct dContact, surface} ptr
             geom    <- #{peek struct dContact, geom} ptr
             fdir    <- peekVector3 (#{ptr struct dContact, fdir1} ptr)
             return (ContactInfo surface geom fdir)
    poke ptr info
        = do #{poke struct dContact, surface} ptr (contactSurface info)
             #{poke struct dContact, geom} ptr (contactGeom info)
             pokeVector3 (#{ptr struct dContact,fdir1} ptr) (contactFDir1 info)

instance Storable Surface where
    sizeOf _ = #{size struct dSurfaceParameters}
    alignment _ = alignment (undefined :: ODEreal)
    peek ptr
        = do mode <- fmap (fromBitmask fromSurfaceMode) (#{peek struct dSurfaceParameters, mode} ptr)
             let mbPeek flag action
                     | flag `elem` mode = fmap Just action
                     | otherwise        = return Nothing
             mu  <- #{peek struct dSurfaceParameters, mu} ptr
             mu2 <- mbPeek HaveMu2 $ #{peek struct dSurfaceParameters, mu2} ptr
             bounce <- mbPeek HaveBounce $ liftM2 (,) (#{peek struct dSurfaceParameters, bounce} ptr)
                                                      (#{peek struct dSurfaceParameters, bounce_vel} ptr)
             softERP <- mbPeek HaveSoftERP $ #{peek struct dSurfaceParameters, soft_erp} ptr
             softCFM <- mbPeek HaveSoftCFM $ #{peek struct dSurfaceParameters, soft_cfm} ptr
             motion1 <- mbPeek HaveMotion1 $ #{peek struct dSurfaceParameters, motion1} ptr
             motion2 <- mbPeek HaveMotion2 $ #{peek struct dSurfaceParameters, motion2} ptr
             slip1 <- mbPeek HaveSlip1 $ #{peek struct dSurfaceParameters, slip1} ptr
             slip2 <- mbPeek HaveSlip2 $ #{peek struct dSurfaceParameters, slip2} ptr
             return (Surface mu mu2 bounce softERP softCFM motion1 motion2 slip1 slip2)
    poke ptr surface
        = do #{poke struct dSurfaceParameters, mode} ptr (toBitmask fromSurfaceMode flags)
             #{poke struct dSurfaceParameters, mu} ptr (surfaceMu surface)
             mbPoke surfaceMu2 $ #{poke struct dSurfaceParameters, mu2} ptr
             mbPoke surfaceBounce $ \(bounce,vel) -> #{poke struct dSurfaceParameters, bounce} ptr bounce >>
                                                     #{poke struct dSurfaceParameters, bounce_vel} ptr vel
             mbPoke surfaceSoftERP $ #{poke struct dSurfaceParameters, soft_erp} ptr
             mbPoke surfaceSoftCFM $ #{poke struct dSurfaceParameters, soft_cfm} ptr
             mbPoke surfaceMotion1 $ #{poke struct dSurfaceParameters, motion1} ptr
             mbPoke surfaceMotion2 $ #{poke struct dSurfaceParameters, motion2} ptr
             mbPoke surfaceSlip1 $ #{poke struct dSurfaceParameters, slip1} ptr
             mbPoke surfaceSlip2 $ #{poke struct dSurfaceParameters, slip2} ptr
        where flags = foldr mkFlag (maybe [] (const [HaveBounce]) (surfaceBounce surface))
                      (zip (HaveMu2 : [ HaveSoftERP .. HaveApprox12])
                           [ surfaceMu2, surfaceSoftERP
                           , surfaceSoftCFM, surfaceMotion1, surfaceMotion2
                           , surfaceSlip1, surfaceSlip2 ])
              mkFlag (flag,fn)
                  | isJust (fn surface) = (:) flag
                  | otherwise = id
              mbPoke fn action
                  = case fn surface of
                      Just val -> action val
                      Nothing  -> return ()

addressOfGeom :: Ptr ContactInfo -> Ptr ContactGeom
addressOfGeom = #{ptr struct dContact, geom}

toBitmask :: (Num b, Bits b) => (a -> b) -> [a] -> b
toBitmask from = foldr (.|.) 0 . map from

fromBitmask :: (Enum a, Bounded a, Num b, Bits b) => (a -> b) -> b -> [a]
fromBitmask from mask = foldr worker [] lst
    where lst = [minBound .. maxBound]
          worker v
              = if (mask .&. from v) /= 0
                   then (:) v
                   else id

toSurfaceMode :: Int -> SurfaceMode
toSurfaceMode #{const dContactMu2} = HaveMu2
toSurfaceMode #{const dContactFDir1} = HaveFDir1
toSurfaceMode #{const dContactBounce} = HaveBounce
toSurfaceMode #{const dContactSoftERP} = HaveSoftERP
toSurfaceMode #{const dContactSoftCFM} = HaveSoftCFM
toSurfaceMode #{const dContactMotion1} = HaveMotion1
toSurfaceMode #{const dContactMotion2} = HaveMotion2
toSurfaceMode #{const dContactSlip1} = HaveSlip1
toSurfaceMode #{const dContactSlip2} = HaveSlip2
toSurfaceMode #{const dContactApprox1_1} = HaveApprox11
toSurfaceMode #{const dContactApprox1_2} = HaveApprox12
toSurfaceMode _ = error "Physics.ODE.Hsc.toSurfaceMode: bad argument"

fromSurfaceMode :: SurfaceMode -> Int
fromSurfaceMode HaveMu2 = #{const dContactMu2}
fromSurfaceMode HaveFDir1 = #{const dContactFDir1}
fromSurfaceMode HaveBounce = #{const dContactBounce}
fromSurfaceMode HaveSoftERP = #{const dContactSoftERP}
fromSurfaceMode HaveSoftCFM = #{const dContactSoftCFM}
fromSurfaceMode HaveMotion1 = #{const dContactMotion1}
fromSurfaceMode HaveMotion2 = #{const dContactMotion2}
fromSurfaceMode HaveSlip1 = #{const dContactSlip1}
fromSurfaceMode HaveSlip2 = #{const dContactSlip2}
fromSurfaceMode HaveApprox11 = #{const dContactApprox1_1}
fromSurfaceMode HaveApprox12 = #{const dContactApprox1_2}

sizeOfMass :: Int
sizeOfMass = #{size struct dMass}

sizeOfMatrix3 :: Int
sizeOfMatrix3 = sizeOf (undefined::ODEreal)*4*3

sizeOfMatrix4 :: Int
sizeOfMatrix4 = sizeOf (undefined::ODEreal)*4*4

peekMass :: Ptr MassStruct -> IO ODEreal
peekMass = #{peek struct dMass, mass}

toJointType :: Int -> JointType
toJointType #{const dJointTypeBall} = Ball
toJointType #{const dJointTypeHinge} = Hinge
toJointType #{const dJointTypeSlider} = Slider
toJointType #{const dJointTypeContact} = Contact
toJointType #{const dJointTypeUniversal} = Universal
toJointType #{const dJointTypeHinge2} = Hinge2
toJointType #{const dJointTypeFixed} = Fixed
toJointType #{const dJointTypeAMotor} = AMotor
toJointType _ = error "Physics.ODE.Hsc.toJointType: bad argument"

fromJointType :: JointType -> Int
fromJointType Ball = #{const dJointTypeBall}
fromJointType Hinge = #{const dJointTypeHinge}
fromJointType Slider = #{const dJointTypeSlider}
fromJointType Contact = #{const dJointTypeContact}
fromJointType Universal = #{const dJointTypeUniversal}
fromJointType Hinge2 = #{const dJointTypeHinge2}
fromJointType Fixed = #{const dJointTypeFixed}
fromJointType AMotor = #{const dJointTypeAMotor}

toBodyIndex :: Int -> BodyIndex
toBodyIndex 0 = First
toBodyIndex 1 = Second
toBodyIndex _ = error "Physics.ODE.Hsc.toBodyIndex: bad argument"

fromBodyIndex :: BodyIndex -> Int
fromBodyIndex First = 0
fromBodyIndex Second = 1

toGeomClass :: Int -> GeomClass
toGeomClass #{const dSphereClass} = Sphere
toGeomClass #{const dBoxClass} = Box
toGeomClass #{const dCCylinderClass} = CappedCylinder
toGeomClass #{const dCylinderClass} = Cylinder
toGeomClass #{const dPlaneClass} = Plane
toGeomClass #{const dGeomTransformClass} = GeomTransform
toGeomClass #{const dRayClass} = Ray
toGeomClass #{const dTriMeshClass} = TriangleMesh
toGeomClass #{const dSimpleSpaceClass} = SimpleSpace
toGeomClass #{const dHashSpaceClass} = HashSpace
toGeomClass _ = error "Physics.ODE.Hsc.toGeomClass: bad argument"

fromGeomClass :: GeomClass -> Int
fromGeomClass Sphere = #{const dSphereClass}
fromGeomClass Box = #{const dBoxClass}
fromGeomClass CappedCylinder = #{const dCCylinderClass}
fromGeomClass Cylinder = #{const dCylinderClass}
fromGeomClass Plane = #{const dPlaneClass}
fromGeomClass GeomTransform = #{const dGeomTransformClass}
fromGeomClass Ray = #{const dRayClass}
fromGeomClass TriangleMesh = #{const dTriMeshClass}
fromGeomClass SimpleSpace = #{const dSimpleSpaceClass}
fromGeomClass HashSpace = #{const dHashSpaceClass}
