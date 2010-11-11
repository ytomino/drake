pragma License (Unrestricted);
with Ada.Numerics.Generic_Elementary_Functions;
package Ada.Numerics.Long_Long_Elementary_Functions is
   new Generic_Elementary_Functions (Long_Long_Float);
pragma Pure (Ada.Numerics.Long_Long_Elementary_Functions);
