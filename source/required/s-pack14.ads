pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_14 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Bits_14 is mod 2 ** 14;
   for Bits_14'Size use 14;

   package Indexing is new Packed_Arrays.Indexing (Bits_14);

   --  required for accessing aligned arrays by compiler
   function Get_14 (Arr : Address; N : Natural) return Bits_14
      renames Indexing.Get;
   procedure Set_14 (Arr : Address; N : Natural; E : Bits_14)
      renames Indexing.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_14 (Arr : Address; N : Natural) return Bits_14
      renames Indexing.Get;
   procedure SetU_14 (Arr : Address; N : Natural; E : Bits_14)
      renames Indexing.Set;

end System.Pack_14;
