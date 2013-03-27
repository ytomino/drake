pragma License (Unrestricted);
--  implementation package required by compiler
with System.Generic_Compare_Arrays;
package System.Compare_Array_Signed_32 is
   pragma Pure;

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Integer_32'Size use 32;

   package Arrays is new Generic_Compare_Arrays (Integer_32);

   --  required to compare arrays by compiler (s-casi32.ads)
   function Compare_Array_S32 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Arrays.Compare;

end System.Compare_Array_Signed_32;
