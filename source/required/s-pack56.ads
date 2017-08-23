pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_56 is
   pragma Preelaborate;
      --  It can not be Pure, subprograms would become __attribute__((const)).

   type Bits_56 is mod 2 ** 56;
   for Bits_56'Size use 56;

   package Indexing is new Packed_Arrays.Indexing (Bits_56);

   --  required for accessing arrays by compiler
   function Get_56 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_56
      renames Indexing.Get;
   procedure Set_56 (
      Arr : Address;
      N : Natural;
      E : Bits_56;
      Rev_SSO : Boolean)
      renames Indexing.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_56 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_56
      renames Indexing.Get;
   procedure SetU_56 (
      Arr : Address;
      N : Natural;
      E : Bits_56;
      Rev_SSO : Boolean)
      renames Indexing.Set;

end System.Pack_56;
