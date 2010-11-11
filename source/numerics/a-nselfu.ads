pragma License (Unrestricted);
with Ada.Numerics.Generic_Elementary_Functions;
package Ada.Numerics.Short_Elementary_Functions is
   new Generic_Elementary_Functions (Short_Float);
pragma Pure (Ada.Numerics.Short_Elementary_Functions);
