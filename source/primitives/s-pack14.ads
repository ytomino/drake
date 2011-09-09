pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Generic_Packed_Arrays;
package System.Pack_14 is
   pragma Pure;

   type Bits_14 is mod 2 ** 14;
   for Bits_14'Size use 14;

   package Arrays is new Generic_Packed_Arrays (Bits_14);

   --  required for accessing aligned arrays by compiler
   function Get_14 (Arr : Address; N : Natural) return Bits_14
      renames Arrays.Get;
   procedure Set_14 (Arr : Address; N : Natural; E : Bits_14)
      renames Arrays.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_14 (Arr : Address; N : Natural) return Bits_14
      renames Arrays.Get;
   procedure SetU_14 (Arr : Address; N : Natural; E : Bits_14)
      renames Arrays.Set;

end System.Pack_14;
