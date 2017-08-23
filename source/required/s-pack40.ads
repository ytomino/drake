pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_40 is
   pragma Preelaborate;
      --  It can not be Pure, subprograms would become __attribute__((const)).

   type Bits_40 is mod 2 ** 40;
   for Bits_40'Size use 40;

   package Indexing is new Packed_Arrays.Indexing (Bits_40);

   --  required for accessing arrays by compiler
   function Get_40 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_40
      renames Indexing.Get;
   procedure Set_40 (
      Arr : Address;
      N : Natural;
      E : Bits_40;
      Rev_SSO : Boolean)
      renames Indexing.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_40 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_40
      renames Indexing.Get;
   procedure SetU_40 (
      Arr : Address;
      N : Natural;
      E : Bits_40;
      Rev_SSO : Boolean)
      renames Indexing.Set;

end System.Pack_40;
