module Physics.ODE.Overloading where

import Physics.ODE.Types
import Data.Typeable
import Foreign

class HasDestroy a where
    destroy :: a -> IO ()

class IsPlaceable a where
    getPosition :: a -> IO (ODEreal, ODEreal, ODEreal)
    setPosition :: a -> ODEreal -> ODEreal -> ODEreal -> IO ()
    getQuaternion :: a -> IO (ODEreal, ODEreal, ODEreal, ODEreal)
    setQuaternion :: a -> (ODEreal, ODEreal, ODEreal, ODEreal) -> IO ()
    getRotation :: a -> IO Matrix3
    setRotation :: a -> Matrix3 -> IO ()

class HasData a where
    setRawData :: a -> Ptr b -> IO ()
    setData :: a -> b -> IO ()
    setSafeData :: Typeable b => a -> b -> IO ()
    getRawData :: a -> IO (Ptr b)
    getData :: a -> IO b
    getSafeData :: Typeable b => a -> IO b
    tryGetSafeData :: Typeable b => a -> IO (Maybe b)

class HasEnable a where
    enable :: a -> IO ()
    disable :: a -> IO ()
    isEnabled :: a -> IO Bool
