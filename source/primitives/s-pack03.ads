pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_03 is
   pragma Pure;

   type Bits_03 is mod 2 ** 3;
   for Bits_03'Size use 3;

   package Arrays is new Packed_Arrays (Bits_03);

   --  required for accessing arrays by compiler
   function Get_03 (Arr : Address; N : Natural) return Bits_03
      renames Arrays.Get;
   procedure Set_03 (Arr : Address; N : Natural; E : Bits_03)
      renames Arrays.Set;

end System.Pack_03;
