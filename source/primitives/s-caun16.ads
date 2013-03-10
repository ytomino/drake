pragma License (Unrestricted);
--  implementation package required by compiler
with System.Generic_Compare_Arrays;
package System.Compare_Array_Unsigned_16 is
   pragma Pure;

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;

   package Arrays is new Generic_Compare_Arrays (Unsigned_16);

   --  required to compare arrays by compiler (s-caun16.ads)
   function Compare_Array_U16 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Arrays.Compare;

end System.Compare_Array_Unsigned_16;
