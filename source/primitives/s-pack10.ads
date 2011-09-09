pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Generic_Packed_Arrays;
package System.Pack_10 is
   pragma Pure;

   type Bits_10 is mod 2 ** 10;
   for Bits_10'Size use 10;

   package Arrays is new Generic_Packed_Arrays (Bits_10);

   --  required for accessing aligned arrays by compiler
   function Get_10 (Arr : Address; N : Natural) return Bits_10
      renames Arrays.Get;
   procedure Set_10 (Arr : Address; N : Natural; E : Bits_10)
      renames Arrays.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_10 (Arr : Address; N : Natural) return Bits_10
      renames Arrays.Get;
   procedure SetU_10 (Arr : Address; N : Natural; E : Bits_10)
      renames Arrays.Set;

end System.Pack_10;
