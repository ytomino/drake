pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_48 is
   pragma Preelaborate;
      --  It can not be Pure, subprograms would become __attribute__((const)).

   type Bits_48 is mod 2 ** 48;
   for Bits_48'Size use 48;

   package Indexing is new Packed_Arrays.Indexing (Bits_48);

   --  required for accessing arrays by compiler
   function Get_48 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_48
      renames Indexing.Get;
   procedure Set_48 (
      Arr : Address;
      N : Natural;
      E : Bits_48;
      Rev_SSO : Boolean)
      renames Indexing.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_48 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_48
      renames Indexing.Get;
   procedure SetU_48 (
      Arr : Address;
      N : Natural;
      E : Bits_48;
      Rev_SSO : Boolean)
      renames Indexing.Set;

end System.Pack_48;
