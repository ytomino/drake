pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Types;
package Ada.Numerics.Long_Long_Complex_Types is
   new Generic_Complex_Types (Long_Long_Float);
pragma Pure (Ada.Numerics.Long_Long_Complex_Types);
