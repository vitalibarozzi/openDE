{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Physics.ODE
    ( module Physics.ODE.Overloading
    , module Physics.ODE.Types 
    ) where

import Physics.ODE.Overloading
import Physics.ODE.Types ( World, Space, Body, Geom, Joint, JointGroup, Mass
                         , ContactGeom (..), ContactInfo (..), Surface (..)
                         , JointType (..), BodyIndex (..), GeomClass (..)
                         , ODEreal )
import Physics.ODE.World as World
import Physics.ODE.Body as Body
import Physics.ODE.Geom as Geom
import Physics.ODE.Joint as Joint
import Physics.ODE.Space as Space
import Physics.ODE.Mass as Mass

instance HasDestroy World where
    destroy = World.destroyWorld
instance HasDestroy Geom where
    destroy = Geom.destroyGeom
instance HasDestroy Space where
    destroy = Space.destroySpace
instance HasDestroy Body where
    destroy = Body.destroyBody
instance HasDestroy Joint where
    destroy = Joint.destroyJoint
instance HasDestroy JointGroup where
    destroy = Joint.destroyGroup
instance HasDestroy Mass where
    destroy = Mass.destroyMass

instance IsPlaceable Body where
    getPosition = Body.getBodyPosition
    setPosition = Body.setBodyPosition
    getQuaternion = Body.getBodyQuaternion
    setQuaternion = Body.setBodyQuaternion
    getRotation = Body.getBodyRotation
    setRotation = Body.setBodyRotation
instance IsPlaceable Geom where
    getPosition = Geom.getGeomPosition
    setPosition = Geom.setGeomPosition
    getQuaternion = Geom.getGeomQuaternion
    setQuaternion = Geom.setGeomQuaternion
    getRotation = Geom.getGeomRotation
    setRotation = Geom.setGeomRotation

instance HasData Body where
    setRawData = Body.setRawBodyData
    setData = Body.setBodyData
    setSafeData = Body.setSafeBodyData
    getRawData = Body.getRawBodyData
    getData = Body.getBodyData
    getSafeData = Body.getSafeBodyData
    tryGetSafeData = Body.tryGetSafeBodyData
instance HasData Geom where
    setRawData = Geom.setRawGeomData
    setData = Geom.setGeomData
    setSafeData = Geom.setSafeGeomData
    getRawData = Geom.getRawGeomData
    getData = Geom.getGeomData
    getSafeData = Geom.getSafeGeomData
    tryGetSafeData = Geom.tryGetSafeGeomData
instance HasData Joint where
    setRawData = Joint.setRawJointData
    setData = Joint.setJointData
    setSafeData = Joint.setSafeJointData
    getRawData = Joint.getRawJointData
    getData = Joint.getJointData
    getSafeData = Joint.getSafeJointData
    tryGetSafeData = Joint.tryGetSafeJointData

instance HasEnable Body where
    enable = Body.enableBody
    disable = Body.disableBody
    isEnabled =Body.isBodyEnabled
instance HasEnable Geom where
    enable = Geom.enableGeom
    disable = Geom.disableGeom
    isEnabled = Geom.isGeomEnabled

