pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Generic_Packed_Arrays;
package System.Pack_13 is
   pragma Pure;

   type Bits_13 is mod 2 ** 13;
   for Bits_13'Size use 13;

   package Arrays is new Generic_Packed_Arrays (Bits_13);

   --  required for accessing arrays by compiler
   function Get_13 (Arr : Address; N : Natural) return Bits_13
      renames Arrays.Get;
   procedure Set_13 (Arr : Address; N : Natural; E : Bits_13)
      renames Arrays.Set;

end System.Pack_13;
