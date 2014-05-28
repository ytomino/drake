pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_11 is
   pragma Pure;

   type Bits_11 is mod 2 ** 11;
   for Bits_11'Size use 11;

   package Indexing is new Packed_Arrays.Indexing (Bits_11);

   --  required for accessing arrays by compiler
   function Get_11 (Arr : Address; N : Natural) return Bits_11
      renames Indexing.Get;
   procedure Set_11 (Arr : Address; N : Natural; E : Bits_11)
      renames Indexing.Set;

end System.Pack_11;
