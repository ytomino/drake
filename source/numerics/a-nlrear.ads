pragma License (Unrestricted);
with Ada.Numerics.Generic_Real_Arrays;
package Ada.Numerics.Long_Real_Arrays is
   new Generic_Real_Arrays (Long_Float);
pragma Pure (Ada.Numerics.Long_Real_Arrays);
