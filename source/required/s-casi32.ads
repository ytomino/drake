pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Signed_32 is
   pragma Preelaborate;
      --  It can not be Pure, subprograms would become __attribute__((const)).

   type Integer_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Integer_32'Size use 32;
   for Integer_32'Alignment use 1;

   package Ordering is new Packed_Arrays.Ordering (Integer_32);

   --  required to compare arrays by compiler (s-casi32.ads)
   function Compare_Array_S32 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

end System.Compare_Array_Signed_32;
