pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Generic_Packed_Arrays;
package System.Pack_06 is
   pragma Pure;

   type Bits_06 is mod 2 ** 6;
   for Bits_06'Size use 6;

   package Arrays is new Generic_Packed_Arrays (Bits_06);

   --  required for accessing aligned arrays by compiler
   function Get_06 (Arr : Address; N : Natural) return Bits_06
      renames Arrays.Get;
   procedure Set_06 (Arr : Address; N : Natural; E : Bits_06)
      renames Arrays.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_06 (Arr : Address; N : Natural) return Bits_06
      renames Arrays.Get;
   procedure SetU_06 (Arr : Address; N : Natural; E : Bits_06)
      renames Arrays.Set;

end System.Pack_06;
