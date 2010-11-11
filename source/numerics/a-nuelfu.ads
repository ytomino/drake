pragma License (Unrestricted);
with Ada.Numerics.Generic_Elementary_Functions;
package Ada.Numerics.Elementary_Functions is
   new Generic_Elementary_Functions (Float);
pragma Pure (Ada.Numerics.Elementary_Functions);
