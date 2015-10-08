pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Unsigned_32 is
   pragma Preelaborate;
      --  It can not be Pure, subprograms would become __attribute__((const)).

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;
   for Unsigned_32'Alignment use 1;

   package Ordering is new Packed_Arrays.Ordering (Unsigned_32);

   --  required to compare arrays by compiler (s-caun32.ads)
   function Compare_Array_U32 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

end System.Compare_Array_Unsigned_32;
