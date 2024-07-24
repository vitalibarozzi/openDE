{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Physics.ODE.Raw.Hsc
    ( addressOfGeom
    , sizeOfMatrix3
    , toJointType
    , fromJointType
    , toBodyIndex
    , fromBodyIndex
    , toGeomClass
    ) where

import Physics.ODE.Raw.Types
import Physics.ODE.Utilities
import Foreign
import Control.Monad
import Data.Maybe

-----------------------------------------------------------
instance Storable ContactInfo where
    sizeOf _ = 248
    alignment _ = alignment (let x = x in x :: Float)
    peek ptr = do 
        surface <- (`peekByteOff` 0)   ptr
        geom    <- (`peekByteOff` 120) ptr
        fdir    <- peekVector3 ((`plusPtr` 216) ptr)
        return (ContactInfo surface geom fdir)
    poke ptr info = do 
        (`pokeByteOff` 0) ptr (contactSurface info)
        (`pokeByteOff` 120) ptr (contactGeom info)
        pokeVector3 ((`plusPtr` 216) ptr) (contactFDir1 info)


-----------------------------------------------------------
--struct dContactGeom {
--   0 dVector3 pos;    // contact position
--    dVector3 normal; // normal vector
--    dReal depth;     // penetration depth
--    dGeomID g1,g2;   // the colliding geoms
--};
instance Storable ContactGeom where
    sizeOf _ = 96
    alignment _ = 4
    peek ptr = do 
        pos    <- peekVector3 ((`plusPtr` 0) ptr)
        normal <- peekVector3 ((`plusPtr` 32) ptr)
        depth  <- peek        ((`plusPtr` 64) ptr)
        obj1   <- peek        ((`plusPtr` 72) ptr) -- TODO are these broken? so why is returning 0?
        obj2   <- peek        ((`plusPtr` 80) ptr) -- TODO are these broken? so why is returning 0?
        return (ContactGeom pos normal depth (obj1,obj2))
    poke ptr cGeom = do 
        pokeVector3 ((`plusPtr` 0) ptr) (contactPos cGeom)
        pokeVector3 ((`plusPtr` 32) ptr) (contactNormal cGeom)
        (`pokeByteOff`          64) ptr (contactDepth cGeom)
        (`pokeByteOff`          72) ptr (fst (contactObjects cGeom))
        (`pokeByteOff`          80) ptr (snd (contactObjects cGeom))

-----------------------------------------------------------
{-
     int mode;

     8 OK dReal mu; 
     16 OK dReal mu2;

     24 dReal rho;
     32 dReal rho2;
     40 dReal rhoN;

     48 OK dReal bounce;
     56 OK dReal bounce_vel;

     64 OK dReal soft_erp;
     72 OK dReal soft_cfm;

     80 OK dReal motion1, motion2, motionN;

     104 OK dReal slip1, slip2;

};
-}
instance Storable Surface where
    sizeOf _ = 120
    alignment _ = alignment (undefined :: Float)
    peek ptr = do
         mode <- fmap (fromBitmask fromSurfaceMode) ((`peekByteOff` 0) ptr)
         let mbPeek flag action
                 | flag `elem` mode = fmap Just action
                 | otherwise        = return Nothing
         mu  <- (`peekByteOff` 8) ptr
         mu2 <- mbPeek HaveMu2 $ (`peekByteOff` 16) ptr
         bounce <- mbPeek HaveBounce $ liftM2 (,) ((`peekByteOff` 48) ptr)
                                                  ((`peekByteOff` 56) ptr)
         softERP <- mbPeek HaveSoftERP $ (`peekByteOff` 64) ptr
         softCFM <- mbPeek HaveSoftCFM $ (`peekByteOff` 72) ptr
         motion1 <- mbPeek HaveMotion1 $ (`peekByteOff` 80) ptr
         motion2 <- mbPeek HaveMotion2 $ (`peekByteOff` 88) ptr
         slip1 <- mbPeek HaveSlip1 $ (`peekByteOff` 104) ptr
         slip2 <- mbPeek HaveSlip2 $ (`peekByteOff` 112) ptr
         return (Surface mode mu mu2 bounce softERP softCFM motion1 motion2 slip1 slip2)
    poke ptr surface = do
         --(`pokeByteOff` 0) ptr (fromSurfaceMode HaveBounce) -- toBitmask fromSurfaceMode flags)
         (`pokeByteOff` 0) ptr (toBitmask fromSurfaceMode flags)
         (`pokeByteOff` 8) ptr (surfaceMu surface)
         mbPoke surfaceMu2 $ (`pokeByteOff` 16) ptr
         mbPoke surfaceBounce $ \(bounce,vel) -> (`pokeByteOff` 48) ptr bounce >> (`pokeByteOff` 56) ptr vel
         mbPoke surfaceSoftERP $ (`pokeByteOff` 64) ptr
         mbPoke surfaceSoftCFM $ (`pokeByteOff` 72) ptr
         mbPoke surfaceMotion1 $ (`pokeByteOff` 80) ptr
         mbPoke surfaceMotion2 $ (`pokeByteOff` 88) ptr
         mbPoke surfaceSlip1 $ (`pokeByteOff` 104) ptr
         mbPoke surfaceSlip2 $ (`pokeByteOff` 112) ptr
      where 
         flags = foldr mkFlag (maybe [] (const [HaveBounce]) (surfaceBounce surface))
                      (zip (HaveMu2 : [ HaveSoftERP .. HaveApprox12])
                           [ surfaceMu2, surfaceSoftERP
                           , surfaceSoftCFM, surfaceMotion1, surfaceMotion2
                           , surfaceSlip1, surfaceSlip2 ])
         mkFlag (flag,fn)
                  | isJust (fn surface) = (:) flag
                  | otherwise = id
         mbPoke fn = forM_ (fn surface)

-----------------------------------------------------------
addressOfGeom :: Ptr ContactInfo -> Ptr ContactGeom
addressOfGeom hsc_ptr = hsc_ptr `plusPtr` 120

-----------------------------------------------------------
toBitmask :: (Num b, Bits b) => (a -> b) -> [a] -> b
toBitmask from = foldr ((.|.) . from) 0

-----------------------------------------------------------
fromBitmask :: forall a b. (Num b, Enum a, Bounded a,Bits b) => (a -> b) -> b -> [a]
fromBitmask from mask = foldr worker [] lst
    where lst = [minBound .. maxBound]
          worker v
              = if mask .&. from v /= (0 :: b)
                   then (:) v
                   else id

-----------------------------------------------------------
{-
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
-}

-----------------------------------------------------------
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

-----------------------------------------------------------
sizeOfMatrix3 :: Int
sizeOfMatrix3 = sizeOf (undefined::Float)*4*3

-----------------------------------------------------------
--sizeOfMatrix4 :: Int
--sizeOfMatrix4 = sizeOf (undefined::Float)*4*4

-----------------------------------------------------------
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

-----------------------------------------------------------
fromJointType :: JointType -> Int
fromJointType Ball = 1
fromJointType Hinge = 2
fromJointType Slider = 3
fromJointType Contact = 4
fromJointType Universal = 5
fromJointType Hinge2 = 6
fromJointType Fixed = 7
fromJointType AMotor = 9

-----------------------------------------------------------
toBodyIndex :: Int -> BodyIndex
toBodyIndex 0 = First
toBodyIndex 1 = Second
toBodyIndex _ = error "Physics.ODE.Hsc.toBodyIndex: bad argument"

-----------------------------------------------------------
fromBodyIndex :: BodyIndex -> Int
fromBodyIndex First = 0
fromBodyIndex Second = 1

-----------------------------------------------------------
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

-----------------------------------------------------------
{-
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
-}
