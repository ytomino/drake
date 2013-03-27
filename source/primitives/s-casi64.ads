pragma License (Unrestricted);
--  implementation package required by compiler
with System.Generic_Compare_Arrays;
package System.Compare_Array_Signed_64 is
   pragma Pure;

   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for Integer_64'Size use 64;

   package Arrays is new Generic_Compare_Arrays (Integer_64);

   --  required to compare arrays by compiler (s-casi64.ads)
   function Compare_Array_S64 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Arrays.Compare;

end System.Compare_Array_Signed_64;
