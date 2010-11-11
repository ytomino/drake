pragma License (Unrestricted);
with Ada.Numerics.Generic_Real_Arrays;
package Ada.Numerics.Short_Real_Arrays is
   new Generic_Real_Arrays (Short_Float);
pragma Pure (Ada.Numerics.Short_Real_Arrays);
