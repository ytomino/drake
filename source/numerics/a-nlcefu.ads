pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Numerics.Long_Complex_Types;
package Ada.Numerics.Long_Complex_Elementary_Functions is
   new Generic_Complex_Elementary_Functions (Long_Complex_Types);
pragma Pure (Ada.Numerics.Long_Complex_Elementary_Functions);
