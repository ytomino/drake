pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Unsigned_8 is
   pragma Preelaborate;
      --  It can not be Pure, subprograms would become __attribute__((const)).

   type Unsigned_8 is mod 2 ** 8;
   for Unsigned_8'Size use 8;

   package Ordering is new Packed_Arrays.Ordering (Unsigned_8);

   --  required to compare arrays by compiler (s-carun8.ads)
   function Compare_Array_U8_Unaligned (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

   --  required to compare arrays by compiler (s-carun8.ads)
   function Compare_Array_U8 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

end System.Compare_Array_Unsigned_8;
