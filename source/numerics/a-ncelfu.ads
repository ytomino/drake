pragma License (Unrestricted);
with Ada.Numerics.Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;
package Ada.Numerics.Complex_Elementary_Functions is
   new Generic_Complex_Elementary_Functions (Complex_Types);
pragma Pure (Ada.Numerics.Complex_Elementary_Functions);
