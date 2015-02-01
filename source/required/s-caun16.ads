pragma License (Unrestricted);
--  implementation package required by compiler
with System.Packed_Arrays;
package System.Compare_Array_Unsigned_16 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;
   for Unsigned_16'Alignment use 1;

   package Ordering is new Packed_Arrays.Ordering (Unsigned_16);

   --  required to compare arrays by compiler (s-caun16.ads)
   function Compare_Array_U16 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Ordering.Compare;

end System.Compare_Array_Unsigned_16;
