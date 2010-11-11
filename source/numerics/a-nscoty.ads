pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types;
package Ada.Numerics.Short_Complex_Types is
   new Generic_Complex_Types (Short_Float);
pragma Pure (Ada.Numerics.Short_Complex_Types);
