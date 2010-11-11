pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Long_Complex_Types;
with Ada.Numerics.Long_Real_Arrays;
package Ada.Numerics.Long_Complex_Arrays is
   new Generic_Complex_Arrays (Long_Real_Arrays, Long_Complex_Types);
pragma Pure (Ada.Numerics.Long_Complex_Arrays);
