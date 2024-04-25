{-# LINE 1 "src/Physics/ODE/Hsc.hsc" #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    sizeOf _ = (96)
{-# LINE 29 "src/Physics/ODE/Hsc.hsc" #-}
    alignment _ = 4
    peek ptr
        = do pos    <- peekVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr)
{-# LINE 32 "src/Physics/ODE/Hsc.hsc" #-}
             normal <- peekVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 32) ptr)
{-# LINE 33 "src/Physics/ODE/Hsc.hsc" #-}
             depth  <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 64) ptr)
{-# LINE 34 "src/Physics/ODE/Hsc.hsc" #-}
             obj1   <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 72) ptr)
{-# LINE 35 "src/Physics/ODE/Hsc.hsc" #-}
             obj2   <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 80) ptr)
{-# LINE 36 "src/Physics/ODE/Hsc.hsc" #-}
             return (ContactGeom pos normal depth (obj1,obj2))
    poke ptr cGeom
        = do pokeVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr) (contactPos cGeom)
{-# LINE 39 "src/Physics/ODE/Hsc.hsc" #-}
             pokeVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 32) ptr) (contactNormal cGeom)
{-# LINE 40 "src/Physics/ODE/Hsc.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr (contactDepth cGeom)
{-# LINE 41 "src/Physics/ODE/Hsc.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 72) ptr (fst (contactObjects cGeom))
{-# LINE 42 "src/Physics/ODE/Hsc.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 80) ptr (snd (contactObjects cGeom))
{-# LINE 43 "src/Physics/ODE/Hsc.hsc" #-}

instance Storable ContactInfo where
    sizeOf _ = (248)
{-# LINE 46 "src/Physics/ODE/Hsc.hsc" #-}
    alignment _ = alignment (undefined :: ODEreal)
    peek ptr
        = do surface <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 49 "src/Physics/ODE/Hsc.hsc" #-}
             geom    <- (\hsc_ptr -> peekByteOff hsc_ptr 120) ptr
{-# LINE 50 "src/Physics/ODE/Hsc.hsc" #-}
             fdir    <- peekVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 216) ptr)
{-# LINE 51 "src/Physics/ODE/Hsc.hsc" #-}
             return (ContactInfo surface geom fdir)
    poke ptr info
        = do (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (contactSurface info)
{-# LINE 54 "src/Physics/ODE/Hsc.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 120) ptr (contactGeom info)
{-# LINE 55 "src/Physics/ODE/Hsc.hsc" #-}
             pokeVector3 ((\hsc_ptr -> hsc_ptr `plusPtr` 216) ptr) (contactFDir1 info)
{-# LINE 56 "src/Physics/ODE/Hsc.hsc" #-}

instance Storable Surface where
    sizeOf _ = (120)
{-# LINE 59 "src/Physics/ODE/Hsc.hsc" #-}
    alignment _ = alignment (undefined :: ODEreal)
    peek ptr
        = do mode <- fmap (fromBitmask fromSurfaceMode) ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 62 "src/Physics/ODE/Hsc.hsc" #-}
             let mbPeek flag action
                     | flag `elem` mode = fmap Just action
                     | otherwise        = return Nothing
             mu  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 66 "src/Physics/ODE/Hsc.hsc" #-}
             mu2 <- mbPeek HaveMu2 $ (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 67 "src/Physics/ODE/Hsc.hsc" #-}
             bounce <- mbPeek HaveBounce $ liftM2 (,) ((\hsc_ptr -> peekByteOff hsc_ptr 48) ptr)
{-# LINE 68 "src/Physics/ODE/Hsc.hsc" #-}
                                                      ((\hsc_ptr -> peekByteOff hsc_ptr 56) ptr)
{-# LINE 69 "src/Physics/ODE/Hsc.hsc" #-}
             softERP <- mbPeek HaveSoftERP $ (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 70 "src/Physics/ODE/Hsc.hsc" #-}
             softCFM <- mbPeek HaveSoftCFM $ (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 71 "src/Physics/ODE/Hsc.hsc" #-}
             motion1 <- mbPeek HaveMotion1 $ (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 72 "src/Physics/ODE/Hsc.hsc" #-}
             motion2 <- mbPeek HaveMotion2 $ (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
{-# LINE 73 "src/Physics/ODE/Hsc.hsc" #-}
             slip1 <- mbPeek HaveSlip1 $ (\hsc_ptr -> peekByteOff hsc_ptr 104) ptr
{-# LINE 74 "src/Physics/ODE/Hsc.hsc" #-}
             slip2 <- mbPeek HaveSlip2 $ (\hsc_ptr -> peekByteOff hsc_ptr 112) ptr
{-# LINE 75 "src/Physics/ODE/Hsc.hsc" #-}
             return (Surface mu mu2 bounce softERP softCFM motion1 motion2 slip1 slip2)
    poke ptr surface
        = do (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (toBitmask fromSurfaceMode flags)
{-# LINE 78 "src/Physics/ODE/Hsc.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr (surfaceMu surface)
{-# LINE 79 "src/Physics/ODE/Hsc.hsc" #-}
             mbPoke surfaceMu2 $ (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr
{-# LINE 80 "src/Physics/ODE/Hsc.hsc" #-}
             mbPoke surfaceBounce $ \(bounce,vel) -> (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr bounce >>
{-# LINE 81 "src/Physics/ODE/Hsc.hsc" #-}
                                                     (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr vel
{-# LINE 82 "src/Physics/ODE/Hsc.hsc" #-}
             mbPoke surfaceSoftERP $ (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr
{-# LINE 83 "src/Physics/ODE/Hsc.hsc" #-}
             mbPoke surfaceSoftCFM $ (\hsc_ptr -> pokeByteOff hsc_ptr 72) ptr
{-# LINE 84 "src/Physics/ODE/Hsc.hsc" #-}
             mbPoke surfaceMotion1 $ (\hsc_ptr -> pokeByteOff hsc_ptr 80) ptr
{-# LINE 85 "src/Physics/ODE/Hsc.hsc" #-}
             mbPoke surfaceMotion2 $ (\hsc_ptr -> pokeByteOff hsc_ptr 88) ptr
{-# LINE 86 "src/Physics/ODE/Hsc.hsc" #-}
             mbPoke surfaceSlip1 $ (\hsc_ptr -> pokeByteOff hsc_ptr 104) ptr
{-# LINE 87 "src/Physics/ODE/Hsc.hsc" #-}
             mbPoke surfaceSlip2 $ (\hsc_ptr -> pokeByteOff hsc_ptr 112) ptr
{-# LINE 88 "src/Physics/ODE/Hsc.hsc" #-}
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
{-# LINE 103 "src/Physics/ODE/Hsc.hsc" #-}

toBitmask :: Bits b => (a -> b) -> [a] -> b
toBitmask from = foldr (.|.) 0 . map from

fromBitmask :: forall a b. (Enum a, Bounded a,Bits b) => (a -> b) -> b -> [a]
fromBitmask from mask = foldr worker [] lst
    where lst = [minBound .. maxBound]
          worker v
              = if (mask .&. from v) /= (0 :: b)
                   then (:) v
                   else id

toSurfaceMode :: Int -> SurfaceMode
toSurfaceMode 1 = HaveMu2
{-# LINE 117 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 2 = HaveFDir1
{-# LINE 118 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 4 = HaveBounce
{-# LINE 119 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 8 = HaveSoftERP
{-# LINE 120 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 16 = HaveSoftCFM
{-# LINE 121 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 32 = HaveMotion1
{-# LINE 122 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 64 = HaveMotion2
{-# LINE 123 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 256 = HaveSlip1
{-# LINE 124 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 512 = HaveSlip2
{-# LINE 125 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 4096 = HaveApprox11
{-# LINE 126 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode 8192 = HaveApprox12
{-# LINE 127 "src/Physics/ODE/Hsc.hsc" #-}
toSurfaceMode _ = error "Physics.ODE.Hsc.toSurfaceMode: bad argument"

fromSurfaceMode :: SurfaceMode -> Int
fromSurfaceMode HaveMu2 = 1
{-# LINE 131 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveFDir1 = 2
{-# LINE 132 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveBounce = 4
{-# LINE 133 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveSoftERP = 8
{-# LINE 134 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveSoftCFM = 16
{-# LINE 135 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveMotion1 = 32
{-# LINE 136 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveMotion2 = 64
{-# LINE 137 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveSlip1 = 256
{-# LINE 138 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveSlip2 = 512
{-# LINE 139 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveApprox11 = 4096
{-# LINE 140 "src/Physics/ODE/Hsc.hsc" #-}
fromSurfaceMode HaveApprox12 = 8192
{-# LINE 141 "src/Physics/ODE/Hsc.hsc" #-}

sizeOfMass :: Int
sizeOfMass = (136)
{-# LINE 144 "src/Physics/ODE/Hsc.hsc" #-}

sizeOfMatrix3 :: Int
sizeOfMatrix3 = sizeOf (undefined::ODEreal)*4*3

sizeOfMatrix4 :: Int
sizeOfMatrix4 = sizeOf (undefined::ODEreal)*4*4

peekMass :: Ptr MassStruct -> IO ODEreal
peekMass = (\hsc_ptr -> peekByteOff hsc_ptr 0)
{-# LINE 153 "src/Physics/ODE/Hsc.hsc" #-}

toJointType :: Int -> JointType
toJointType 1 = Ball
{-# LINE 156 "src/Physics/ODE/Hsc.hsc" #-}
toJointType 2 = Hinge
{-# LINE 157 "src/Physics/ODE/Hsc.hsc" #-}
toJointType 3 = Slider
{-# LINE 158 "src/Physics/ODE/Hsc.hsc" #-}
toJointType 4 = Contact
{-# LINE 159 "src/Physics/ODE/Hsc.hsc" #-}
toJointType 5 = Universal
{-# LINE 160 "src/Physics/ODE/Hsc.hsc" #-}
toJointType 6 = Hinge2
{-# LINE 161 "src/Physics/ODE/Hsc.hsc" #-}
toJointType 7 = Fixed
{-# LINE 162 "src/Physics/ODE/Hsc.hsc" #-}
toJointType 9 = AMotor
{-# LINE 163 "src/Physics/ODE/Hsc.hsc" #-}
toJointType _ = error "Physics.ODE.Hsc.toJointType: bad argument"

fromJointType :: JointType -> Int
fromJointType Ball = 1
{-# LINE 167 "src/Physics/ODE/Hsc.hsc" #-}
fromJointType Hinge = 2
{-# LINE 168 "src/Physics/ODE/Hsc.hsc" #-}
fromJointType Slider = 3
{-# LINE 169 "src/Physics/ODE/Hsc.hsc" #-}
fromJointType Contact = 4
{-# LINE 170 "src/Physics/ODE/Hsc.hsc" #-}
fromJointType Universal = 5
{-# LINE 171 "src/Physics/ODE/Hsc.hsc" #-}
fromJointType Hinge2 = 6
{-# LINE 172 "src/Physics/ODE/Hsc.hsc" #-}
fromJointType Fixed = 7
{-# LINE 173 "src/Physics/ODE/Hsc.hsc" #-}
fromJointType AMotor = 9
{-# LINE 174 "src/Physics/ODE/Hsc.hsc" #-}

toBodyIndex :: Int -> BodyIndex
toBodyIndex 0 = First
toBodyIndex 1 = Second
toBodyIndex _ = error "Physics.ODE.Hsc.toBodyIndex: bad argument"

fromBodyIndex :: BodyIndex -> Int
fromBodyIndex First = 0
fromBodyIndex Second = 1

toGeomClass :: Int -> GeomClass
toGeomClass 0 = Sphere
{-# LINE 186 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass 1 = Box
{-# LINE 187 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass 2 = CappedCylinder
{-# LINE 188 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass 3 = Cylinder
{-# LINE 189 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass 4 = Plane
{-# LINE 190 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass 7 = GeomTransform
{-# LINE 191 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass 5 = Ray
{-# LINE 192 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass 8 = TriangleMesh
{-# LINE 193 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass 10 = SimpleSpace
{-# LINE 194 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass 11 = HashSpace
{-# LINE 195 "src/Physics/ODE/Hsc.hsc" #-}
toGeomClass _ = error "Physics.ODE.Hsc.toGeomClass: bad argument"

fromGeomClass :: GeomClass -> Int
fromGeomClass Sphere = 0
{-# LINE 199 "src/Physics/ODE/Hsc.hsc" #-}
fromGeomClass Box = 1
{-# LINE 200 "src/Physics/ODE/Hsc.hsc" #-}
fromGeomClass CappedCylinder = 2
{-# LINE 201 "src/Physics/ODE/Hsc.hsc" #-}
fromGeomClass Cylinder = 3
{-# LINE 202 "src/Physics/ODE/Hsc.hsc" #-}
fromGeomClass Plane = 4
{-# LINE 203 "src/Physics/ODE/Hsc.hsc" #-}
fromGeomClass GeomTransform = 7
{-# LINE 204 "src/Physics/ODE/Hsc.hsc" #-}
fromGeomClass Ray = 5
{-# LINE 205 "src/Physics/ODE/Hsc.hsc" #-}
fromGeomClass TriangleMesh = 8
{-# LINE 206 "src/Physics/ODE/Hsc.hsc" #-}
fromGeomClass SimpleSpace = 10
{-# LINE 207 "src/Physics/ODE/Hsc.hsc" #-}
fromGeomClass HashSpace = 11
{-# LINE 208 "src/Physics/ODE/Hsc.hsc" #-}
