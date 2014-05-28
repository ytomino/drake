pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_12 is
   pragma Pure;

   type Bits_12 is mod 2 ** 12;
   for Bits_12'Size use 12;

   package Arrays is new Packed_Arrays (Bits_12);

   --  required for accessing aligned arrays by compiler
   function Get_12 (Arr : Address; N : Natural) return Bits_12
      renames Arrays.Get;
   procedure Set_12 (Arr : Address; N : Natural; E : Bits_12)
      renames Arrays.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_12 (Arr : Address; N : Natural) return Bits_12
      renames Arrays.Get;
   procedure SetU_12 (Arr : Address; N : Natural; E : Bits_12)
      renames Arrays.Set;

end System.Pack_12;
