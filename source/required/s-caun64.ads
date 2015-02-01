pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Unsigned_64 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;
   for Unsigned_64'Alignment use 1;

   package Ordering is new Packed_Arrays.Ordering (Unsigned_64);

   --  required to compare arrays by compiler (s-caun64.ads)
   function Compare_Array_U64 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

end System.Compare_Array_Unsigned_64;
