pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Signed_16 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Integer_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Integer_16'Size use 16;
   for Integer_16'Alignment use 1;

   package Ordering is new Packed_Arrays.Ordering (Integer_16);

   --  required to compare arrays by compiler (s-casi16.ads)
   function Compare_Array_S16 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

end System.Compare_Array_Signed_16;
