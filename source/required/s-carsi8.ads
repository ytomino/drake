pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Signed_8 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Integer_8 is range -2 ** 7 .. 2 ** 7 - 1;
   for Integer_8'Size use 8;

   package Ordering is new Packed_Arrays.Ordering (Integer_8);

   --  required to compare arrays by compiler (s-carsi8.ads)
   function Compare_Array_S8_Unaligned (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

   --  required to compare arrays by compiler (s-carsi8.ads)
   function Compare_Array_S8 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

end System.Compare_Array_Signed_8;
