-- | "The following is taken directly from ode/error.h, version .11".
module Physics.ODE.Error (
    errorHandler,
    debugHandler,
    messageHandler,
    errorODE,
    debugODE,
    messageODE,
    MessageFunction,
) where

import Physics.ODE.Raw.Error
import Data.StateVar
import Foreign.C.String
import Foreign.Ptr

-----------------------------------------------------------
errorODE :: Int -> String -> IO ()
{-# INLINE errorODE #-}
errorODE n s = do
    cs <- newCString s
    c'dError (fromIntegral n) cs nullPtr

-----------------------------------------------------------
debugODE :: Int -> String -> IO ()
{-# INLINE debugODE #-}
debugODE n s = do
    cs <- newCString s
    c'dDebug (fromIntegral n) cs nullPtr

-----------------------------------------------------------
messageODE :: Int -> String -> IO ()
{-# INLINE messageODE #-}
messageODE n s = do
    cs <- newCString s
    c'dMessage (fromIntegral n) cs nullPtr

-----------------------------------------------------------
errorHandler :: StateVar (FunPtr MessageFunction)
{-# INLINE errorHandler #-}
errorHandler = 
    StateVar _get _set
  where
    _get = pure c'dGetErrorHandler
    _set = c'dSetErrorHandler

-----------------------------------------------------------
debugHandler :: StateVar (FunPtr MessageFunction)
{-# INLINE debugHandler #-}
debugHandler = 
    StateVar _get _set
  where
    _get = pure c'dGetDebugHandler
    _set = c'dSetDebugHandler

-----------------------------------------------------------
messageHandler :: StateVar (FunPtr MessageFunction)
{-# INLINE messageHandler #-}
messageHandler = 
    StateVar _get _set
  where
    _get = pure c'dGetMessageHandler
    _set = c'dSetMessageHandler



