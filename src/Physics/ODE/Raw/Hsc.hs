{-# LANGUAGE ScopedTypeVariables #-}

module Physics.ODE.Raw.Hsc
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

import Physics.ODE.Raw.Types
import Physics.ODE.Raw.Utilities
import Foreign

import Control.Monad
import Data.Maybe

instance Storable ContactGeom where
    sizeOf _ = (96)
    alignment _ = 4
    peek ptr
        = do pos    <- peekVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr)
             normal <- peekVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 32) ptr)
             depth  <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 64) ptr)
             obj1   <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 72) ptr)
             obj2   <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 80) ptr)
             return (ContactGeom pos normal depth (obj1,obj2))
    poke ptr cGeom
        = do pokeVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr) (contactPos cGeom)
             pokeVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 32) ptr) (contactNormal cGeom)
             (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr (contactDepth cGeom)
             (\hsc_ptr -> pokeByteOff hsc_ptr 72) ptr (fst (contactObjects cGeom))
             (\hsc_ptr -> pokeByteOff hsc_ptr 80) ptr (snd (contactObjects cGeom))

instance Storable ContactInfo where
    sizeOf _ = (248)
    alignment _ = alignment (undefined :: ODEreal)
    peek ptr
        = do surface <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
             geom    <- (\hsc_ptr -> peekByteOff hsc_ptr 120) ptr
             fdir    <- peekVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 216) ptr)
             return (ContactInfo surface geom fdir)
    poke ptr info
        = do (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (contactSurface info)
             (\hsc_ptr -> pokeByteOff hsc_ptr 120) ptr (contactGeom info)
             pokeVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 216) ptr) (contactFDir1 info)

instance Storable Surface where
    sizeOf _ = (120)
    alignment _ = alignment (undefined :: ODEreal)
    peek ptr
        = do mode <- fmap (fromBitmask fromSurfaceMode) ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
             let mbPeek flag action
                     | flag `elem` mode = fmap Just action
                     | otherwise        = return Nothing
             mu  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
             mu2 <- mbPeek HaveMu2 $ (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
             bounce <- mbPeek HaveBounce $ liftM2 (,) ((\hsc_ptr -> peekByteOff hsc_ptr 48) ptr)
                                                      ((\hsc_ptr -> peekByteOff hsc_ptr 56) ptr)
             softERP <- mbPeek HaveSoftERP $ (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
             softCFM <- mbPeek HaveSoftCFM $ (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
             motion1 <- mbPeek HaveMotion1 $ (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
             motion2 <- mbPeek HaveMotion2 $ (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
             slip1 <- mbPeek HaveSlip1 $ (\hsc_ptr -> peekByteOff hsc_ptr 104) ptr
             slip2 <- mbPeek HaveSlip2 $ (\hsc_ptr -> peekByteOff hsc_ptr 112) ptr
             return (Surface mu mu2 bounce softERP softCFM motion1 motion2 slip1 slip2)
    poke ptr surface
        = do (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (toBitmask fromSurfaceMode flags)
             (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr (surfaceMu surface)
             mbPoke surfaceMu2 $ (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr
             mbPoke surfaceBounce $ \(bounce,vel) -> (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr bounce >>
                                                     (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr vel
             mbPoke surfaceSoftERP $ (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr
             mbPoke surfaceSoftCFM $ (\hsc_ptr -> pokeByteOff hsc_ptr 72) ptr
             mbPoke surfaceMotion1 $ (\hsc_ptr -> pokeByteOff hsc_ptr 80) ptr
             mbPoke surfaceMotion2 $ (\hsc_ptr -> pokeByteOff hsc_ptr 88) ptr
             mbPoke surfaceSlip1 $ (\hsc_ptr -> pokeByteOff hsc_ptr 104) ptr
             mbPoke surfaceSlip2 $ (\hsc_ptr -> pokeByteOff hsc_ptr 112) ptr
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
addressOfGeom = (\hsc_ptr -> hsc_ptr `plusPtr` 120)

toBitmask :: (Num b, Bits b) => (a -> b) -> [a] -> b
toBitmask from = foldr (.|.) 0 . map from

fromBitmask :: forall a b. (Num b, Enum a, Bounded a,Bits b) => (a -> b) -> b -> [a]
fromBitmask from mask = foldr worker [] lst
    where lst = [minBound .. maxBound]
          worker v
              = if (mask .&. from v) /= (0 :: b)
                   then (:) v
                   else id

toSurfaceMode :: Int -> SurfaceMode
toSurfaceMode 1 = HaveMu2
toSurfaceMode 2 = HaveFDir1
toSurfaceMode 4 = HaveBounce
toSurfaceMode 8 = HaveSoftERP
toSurfaceMode 16 = HaveSoftCFM
toSurfaceMode 32 = HaveMotion1
toSurfaceMode 64 = HaveMotion2
toSurfaceMode 256 = HaveSlip1
toSurfaceMode 512 = HaveSlip2
toSurfaceMode 4096 = HaveApprox11
toSurfaceMode 8192 = HaveApprox12
toSurfaceMode _ = error "Physics.ODE.Hsc.toSurfaceMode: bad argument"

fromSurfaceMode :: SurfaceMode -> Int
fromSurfaceMode HaveMu2 = 1
fromSurfaceMode HaveFDir1 = 2
fromSurfaceMode HaveBounce = 4
fromSurfaceMode HaveSoftERP = 8
fromSurfaceMode HaveSoftCFM = 16
fromSurfaceMode HaveMotion1 = 32
fromSurfaceMode HaveMotion2 = 64
fromSurfaceMode HaveSlip1 = 256
fromSurfaceMode HaveSlip2 = 512
fromSurfaceMode HaveApprox11 = 4096
fromSurfaceMode HaveApprox12 = 8192

sizeOfMass :: Int
sizeOfMass = (136)

sizeOfMatrix3 :: Int
sizeOfMatrix3 = sizeOf (undefined::ODEreal)*4*3

sizeOfMatrix4 :: Int
sizeOfMatrix4 = sizeOf (undefined::ODEreal)*4*4

peekMass :: Ptr MassStruct -> IO ODEreal
peekMass = (\hsc_ptr -> peekByteOff hsc_ptr 0)

toJointType :: Int -> JointType
toJointType 1 = Ball
toJointType 2 = Hinge
toJointType 3 = Slider
toJointType 4 = Contact
toJointType 5 = Universal
toJointType 6 = Hinge2
toJointType 7 = Fixed
toJointType 9 = AMotor
toJointType _ = error "Physics.ODE.Hsc.toJointType: bad argument"

fromJointType :: JointType -> Int
fromJointType Ball = 1
fromJointType Hinge = 2
fromJointType Slider = 3
fromJointType Contact = 4
fromJointType Universal = 5
fromJointType Hinge2 = 6
fromJointType Fixed = 7
fromJointType AMotor = 9

toBodyIndex :: Int -> BodyIndex
toBodyIndex 0 = First
toBodyIndex 1 = Second
toBodyIndex _ = error "Physics.ODE.Hsc.toBodyIndex: bad argument"

fromBodyIndex :: BodyIndex -> Int
fromBodyIndex First = 0
fromBodyIndex Second = 1

toGeomClass :: Int -> GeomClass
toGeomClass 0 = Sphere
toGeomClass 1 = Box
toGeomClass 2 = CappedCylinder
toGeomClass 3 = Cylinder
toGeomClass 4 = Plane
toGeomClass 7 = GeomTransform
toGeomClass 5 = Ray
toGeomClass 8 = TriangleMesh
toGeomClass 10 = SimpleSpace
toGeomClass 11 = HashSpace
toGeomClass _ = error "Physics.ODE.Hsc.toGeomClass: bad argument"

fromGeomClass :: GeomClass -> Int
fromGeomClass Sphere = 0
fromGeomClass Box = 1
fromGeomClass CappedCylinder = 2
fromGeomClass Cylinder = 3
fromGeomClass Plane = 4
fromGeomClass GeomTransform = 7
fromGeomClass Ray = 5
fromGeomClass TriangleMesh = 8
fromGeomClass SimpleSpace = 10
fromGeomClass HashSpace = 11
