pragma License (Unrestricted);
--  implementation package required by compiler
with System.Generic_Compare_Arrays;
package System.Compare_Array_Unsigned_32 is
   pragma Pure;

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;

   package Arrays is new Generic_Compare_Arrays (Unsigned_32);

   --  required to compare arrays by compiler (s-caun32.ads)
   function Compare_Array_U32 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Arrays.Compare;

end System.Compare_Array_Unsigned_32;
