pragma License (Unrestricted);
--  implementation package required by compiler
with System.Generic_Compare_Arrays;
package System.Compare_Array_Signed_16 is
   pragma Pure;

   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;

   package Arrays is new Generic_Compare_Arrays (Integer_16);

   --  required to compare arrays by compiler (s-casi16.ads)
   function Compare_Array_S16 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Arrays.Compare;

end System.Compare_Array_Signed_16;
