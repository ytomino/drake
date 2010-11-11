pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types;
package Ada.Numerics.Complex_Types is
   new Generic_Complex_Types (Float);
pragma Pure (Ada.Numerics.Complex_Types);
