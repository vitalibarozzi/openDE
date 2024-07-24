module Physics.ODE.Raw.Rotation
    ( setIdentity
    , fromAxisAndAngle
    , fromEulerAngles
    ) where

import Physics.ODE.Raw.Types



foreign import ccall unsafe "dRSetIdentity" setIdentity           :: Matrix3 -> IO ()
foreign import ccall unsafe "dRFromAxisAndAngle" fromAxisAndAngle :: Matrix3 -> Float -> Float -> Float -> Float -> IO ()
foreign import ccall unsafe "dRFromEulerAngles" fromEulerAngles   :: Matrix3 -> Float -> Float -> Float -> IO ()



{- foreign import ccall unsafe "dRtoQ" dRtoQ
    :: Matrix3 -> Ptr Float -> IO ()

rToQ :: Matrix3 -> IO Quaternion
rToQ matrix
    = allocaArray 4 $ \ptr ->
      do dRtoQ matrix ptr
         [a,b,c,d] <- peekArray 4 ptr
         return (a,b,c,d)


    w=cos(θ/2)
    (x,y,z)=u⃗ sin(θ/2)
    θ is a rotation angle
    u⃗  is a unit length rotation axis.

void dRSetIdentity (dMatrix3 R);

void dRFromAxisAndAngle (dMatrix3 R, dReal ax, dReal ay, dReal az, dReal angle);

void dRFromEulerAngles (dMatrix3 R, dReal phi, dReal theta, dReal psi);

void dRFrom2Axes (dMatrix3 R, dReal ax, dReal ay, dReal az, dReal bx, dReal by, dReal bz);

void dQSetIdentity (dQuaternion q);

void dQFromAxisAndAngle (dQuaternion q, dReal ax, dReal ay, dReal az, dReal angle);

void dQMultiply0 (dQuaternion qa, const dQuaternion qb, const dQuaternion qc);
void dQMultiply1 (dQuaternion qa, const dQuaternion qb, const dQuaternion qc);
void dQMultiply2 (dQuaternion qa, const dQuaternion qb, const dQuaternion qc);
void dQMultiply3 (dQuaternion qa, const dQuaternion qb, const dQuaternion qc);

void dQtoR (const dQuaternion q, dMatrix3 R);

void dRtoQ (const dMatrix3 R, dQuaternion q);

void dWtoDQ (const dVector3 w, const dQuaternion q, dVector4 dq);
-}
