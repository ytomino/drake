pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_24 is
   pragma Preelaborate;
      --  It can not be Pure, subprograms would become __attribute__((const)).

   type Bits_24 is mod 2 ** 24;
   for Bits_24'Size use 24;

   package Indexing is new Packed_Arrays.Indexing (Bits_24);

   --  required for accessing arrays by compiler
   function Get_24 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_24
      renames Indexing.Get;
   procedure Set_24 (
      Arr : Address;
      N : Natural;
      E : Bits_24;
      Rev_SSO : Boolean)
      renames Indexing.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_24 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_24
      renames Indexing.Get;
   procedure SetU_24 (
      Arr : Address;
      N : Natural;
      E : Bits_24;
      Rev_SSO : Boolean)
      renames Indexing.Set;

end System.Pack_24;
