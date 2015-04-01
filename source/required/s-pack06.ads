pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_06 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Bits_06 is mod 2 ** 6;
   for Bits_06'Size use 6;

   package Indexing is new Packed_Arrays.Indexing (Bits_06);

   --  required for accessing aligned arrays by compiler
   function Get_06 (Arr : Address; N : Natural) return Bits_06
      renames Indexing.Get;
   procedure Set_06 (Arr : Address; N : Natural; E : Bits_06)
      renames Indexing.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_06 (Arr : Address; N : Natural) return Bits_06
      renames Indexing.Get;
   procedure SetU_06 (Arr : Address; N : Natural; E : Bits_06)
      renames Indexing.Set;

end System.Pack_06;
