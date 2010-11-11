pragma License (Unrestricted);
with Ada.Numerics.Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Real_Arrays;
package Ada.Numerics.Complex_Arrays is
   new Generic_Complex_Arrays (Real_Arrays, Complex_Types);
pragma Pure (Ada.Numerics.Complex_Arrays);
