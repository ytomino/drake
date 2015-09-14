pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_07 is
   pragma Preelaborate;
      --  It can not be Pure, subprograms would become __attribute__((const)).

   type Bits_07 is mod 2 ** 7;
   for Bits_07'Size use 7;

   package Indexing is new Packed_Arrays.Indexing (Bits_07);

   --  required for accessing arrays by compiler
   function Get_07 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_07
      renames Indexing.Get;
   procedure Set_07 (
      Arr : Address;
      N : Natural;
      E : Bits_07;
      Rev_SSO : Boolean)
      renames Indexing.Set;

end System.Pack_07;
