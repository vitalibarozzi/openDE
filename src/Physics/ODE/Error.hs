module Physics.ODE.Error (
) where

{-
void dWorldExportDIF (dWorldID w, FILE *file, const char *prefix)

Set file to a file descriptor to print to a file or to stdout to print to the standard output (console).
Error and memory functions

The following is taken directly from ode/error.h, version .11:

All user defined error functions have this type. error and debug functions should not return:

typedef void dMessageFunction (int errnum, const char *msg, va_list ap);

Set a new error, debug or warning handler. if fn is 0, the default handlers are used:

void dSetErrorHandler (dMessageFunction *fn);
void dSetDebugHandler (dMessageFunction *fn);
void dSetMessageHandler (dMessageFunction *fn);

Return the current error, debug or warning handler. if the return value is 0, the default handlers are in place:

dMessageFunction *dGetErrorHandler(void);
dMessageFunction *dGetDebugHandler(void);
dMessageFunction *dGetMessageHandler(void);

Generate a fatal error, debug trap or a message:

ODE_API void dError (int num, const char *msg, ...);
ODE_API void dDebug (int num, const char *msg, ...);
ODE_API void dMessage (int num, const char *msg, ...);

-}
