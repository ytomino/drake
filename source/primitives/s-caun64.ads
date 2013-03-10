pragma License (Unrestricted);
--  implementation package required by compiler
with System.Generic_Compare_Arrays;
package System.Compare_Array_Unsigned_64 is
   pragma Pure;

   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;

   package Arrays is new Generic_Compare_Arrays (Unsigned_64);

   --  required to compare arrays by compiler (s-caun64.ads)
   function Compare_Array_U64 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Arrays.Compare;

end System.Compare_Array_Unsigned_64;
