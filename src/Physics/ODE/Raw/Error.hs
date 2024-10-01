module Physics.ODE.Raw.Error (
    c'dSetErrorHandler,
    c'dSetDebugHandler,
    c'dSetMessageHandler,
    c'dGetErrorHandler,
    c'dGetDebugHandler,
    c'dGetMessageHandler,
    c'dError,
    c'dDebug,
    c'dMessage,
    MessageFunction,
) where

import Foreign.C.Types (CInt(..))
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.C.String (CString)

-- typedef void dMessageFunction (int errnum, const char *msg, va_list ap);
-- void dSetErrorHandler (dMessageFunction *fn);
-- void dSetDebugHandler (dMessageFunction *fn);
-- void dSetMessageHandler (dMessageFunction *fn);
-- dMessageFunction *dGetErrorHandler(void);
-- dMessageFunction *dGetDebugHandler(void);
-- dMessageFunction *dGetMessageHandler(void);
-- ODE_API void dError (int num, const char *msg, ...);
-- ODE_API void dDebug (int num, const char *msg, ...);
-- ODE_API void dMessage (int num, const char *msg, ...);

foreign import ccall unsafe "dSetErrorHandler"     c'dSetErrorHandler      :: FunPtr MessageFunction -> IO ()

foreign import ccall unsafe "dSetDebugHandler"     c'dSetDebugHandler      :: FunPtr MessageFunction -> IO ()

foreign import ccall unsafe "dSetMessageHandler"   c'dSetMessageHandler    :: FunPtr MessageFunction -> IO ()

foreign import ccall unsafe "& dGetErrorHandler"    c'dGetErrorHandler     :: FunPtr MessageFunction

foreign import ccall unsafe "& dGetDebugHandler"    c'dGetDebugHandler     :: FunPtr MessageFunction

foreign import ccall unsafe "& dGetMessageHandler"  c'dGetMessageHandler   :: FunPtr MessageFunction

foreign import ccall unsafe "dError"               c'dError                :: MessageFunction

foreign import ccall unsafe "dDebug"               c'dDebug                :: MessageFunction

foreign import ccall unsafe "dMessage"             c'dMessage              :: MessageFunction

type MessageFunction = CInt -> CString -> Ptr () -> IO ()
