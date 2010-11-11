pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Short_Complex_Types;
with Ada.Numerics.Short_Real_Arrays;
package Ada.Numerics.Short_Complex_Arrays is
   new Generic_Complex_Arrays (Short_Real_Arrays, Short_Complex_Types);
pragma Pure (Ada.Numerics.Short_Complex_Arrays);
