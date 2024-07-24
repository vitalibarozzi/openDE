module Physics.ODE.Raw.Mass where

import Foreign
import Physics.ODE.Raw.Types

foreign import ccall unsafe "dMassAdjust" adjustdMassAdjust    :: Ptr MassStruct -> Float -> IO ()
foreign import ccall unsafe "dMassSetZero" setZerodMassSetZero :: Ptr MassStruct -> IO ()


--  void dMassTranslate (dMass *, dReal x, dReal y, dReal z);
--  void dMassRotate (dMass *, const dMatrix3 R);
--  void dMassAdd (dMass *a, const dMass *b);
--  void dMassSetParameters (dMass *, dReal themass,
--                           dReal cgx, dReal cgy, dReal cgz,
--                           dReal I11, dReal I22, dReal I33,
--                           dReal I12, dReal I13, dReal I23);
--  void dMassSetSphere (dMass *, dReal density, dReal radius);
--  void dMassSetSphereTotal (dMass *, dReal total_mass, dReal radius);
--  void dMassSetCappedCylinder (dMass *, dReal density, int direction,
--                               dReal radius, dReal length);
--  void dMassSetCappedCylinderTotal (dMass *, dReal total_mass, int direction,
--                                    dReal radius, dReal length);
--  void dMassSetCylinder (dMass *, dReal density, int direction,
--                         dReal radius, dReal length);
--  void dMassSetCylinderTotal (dMass *, dReal total_mass, int direction,
--                              dReal radius, dReal length);
--  void dMassSetBox (dMass *, dReal density,
--                    dReal lx, dReal ly, dReal lz);
--  void dMassSetBoxTotal (dMass *, dReal total_mass,
--                         dReal lx, dReal ly, dReal lz);
--  void dMassAdjust (dMass *, dReal newmass);
--  void dMassSetZero (dMass *);

{-
Set all the mass parameters to zero.

void dMassSetParameters (dMass *, dReal themass,
                         dReal cgx, dReal cgy, dReal cgz,
                         dReal I11, dReal I22, dReal I33,
                         dReal I12, dReal I13, dReal I23);

Set the mass parameters to the given values. themass is the mass of the body. (cx,cy,cz) is the center of gravity position in the body frame. The Ixx values are the elements of the inertia matrix: ( I11 I12 I13 ) ( I12 I22 I23 ) ( I13 I23 I33 )

void dMassSetSphere (dMass *, dReal density, dReal radius);
void dMassSetSphereTotal (dMass *, dReal total_mass, dReal radius);

Set the mass parameters to represent a sphere of the given radius and density, with the center of mass at (0,0,0) relative to the body. The first function accepts the density of the sphere, the second accepts the total mass of the sphere.

void dMassSetCapsule (dMass *, dReal density, int direction, dReal radius, dReal length);
void dMassSetCapsuleTotal (dMass *, dReal total_mass, int direction, dReal radius, dReal length);

Set the mass parameters to represent a capsule of the given parameters and density, with the center of mass at (0,0,0) relative to the body. The radius of the cylinder (and the spherical cap) is radius. The length of the cylinder (not counting the spherical cap) is length. The cylinder's long axis is oriented along the body's x, y or z axis according to the value of direction (1=x, 2=y, 3=z). The first function accepts the density of the object, the second accepts its total mass.

void dMassSetCylinder (dMass *, dReal density, int direction, dReal radius, dReal length);
void dMassSetCylinderTotal (dMass *, dReal total_mass, int direction, dReal radius, dReal length);

Set the mass parameters to represent a flat-ended cylinder of the given parameters and density, with the center of mass at (0,0,0) relative to the body. The radius of the cylinder is radius. The length of the cylinder is length. The cylinder's long axis is oriented along the body's x, y or z axis according to the value of direction (1=x, 2=y, 3=z). The first function accepts the density of the object, the second accepts its total mass.

void dMassSetBox (dMass *, dReal density, dReal lx, dReal ly, dReal lz);
void dMassSetBoxTotal (dMass *, dReal total_mass, dReal lx, dReal ly, dReal lz);

Set the mass parameters to represent a box of the given dimensions and density, with the center of mass at (0,0,0) relative to the body. The side lengths of the box along the x, y and z axes are lx, ly and lz. The first function accepts the density of the object, the second accepts its total mass.

void dMassSetTrimesh (dMass *, dReal density, dGeomID g)

Set the mass parameters to represent an arbitrary trimesh of the given geometry and density, location of a trimesh geometry center of mass and set the inertia matrix.

void dMassAdjust (dMass *, dReal newmass);

Given mass parameters for some object, adjust them so the total mass is now newmass. This is useful when using the above functions to set the mass parameters for certain objects - they take the object density, not the total mass.

void dMassTranslate (dMass *, dReal x, dReal y, dReal z);

Given mass parameters for some object, adjust them to represent the object displaced by (x,y,z) relative to the body frame.

void dMassRotate (dMass *, const dMatrix3 R);

Given mass parameters for some object, adjust them to represent the object rotated by R relative to the body frame.

void dMassAdd (dMass *a, const dMass *b);

-}
