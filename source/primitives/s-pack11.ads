pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Generic_Packed_Arrays;
package System.Pack_11 is
   pragma Pure;

   type Bits_11 is mod 2 ** 11;
   for Bits_11'Size use 11;

   package Arrays is new Generic_Packed_Arrays (Bits_11);

   --  required for accessing arrays by compiler
   function Get_11 (Arr : Address; N : Natural) return Bits_11
      renames Arrays.Get;
   procedure Set_11 (Arr : Address; N : Natural; E : Bits_11)
      renames Arrays.Set;

end System.Pack_11;
