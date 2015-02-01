pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Signed_64 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Integer_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for Integer_64'Size use 64;
   for Integer_64'Alignment use 1;

   package Ordering is new Packed_Arrays.Ordering (Integer_64);

   --  required to compare arrays by compiler (s-casi64.ads)
   function Compare_Array_S64 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

end System.Compare_Array_Signed_64;
