pragma License (Unrestricted);
with Ada.Numerics.Generic_Complex_Elementary_Functions;
with Ada.Numerics.Short_Complex_Types;
package Ada.Numerics.Short_Complex_Elementary_Functions is
   new Generic_Complex_Elementary_Functions (Short_Complex_Types);
pragma Pure (Ada.Numerics.Short_Complex_Elementary_Functions);
