pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Generic_Packed_Arrays;
package System.Pack_07 is
   pragma Pure;

   type Bits_07 is mod 2 ** 7;
   for Bits_07'Size use 7;

   package Arrays is new Generic_Packed_Arrays (Bits_07);

   --  required for accessing arrays by compiler
   function Get_07 (Arr : Address; N : Natural) return Bits_07
      renames Arrays.Get;
   procedure Set_07 (Arr : Address; N : Natural; E : Bits_07)
      renames Arrays.Set;

end System.Pack_07;
