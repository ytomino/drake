pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_05 is
   pragma Preelaborate;
      --  It can not be Pure, subprograms would become __attribute__((const)).

   type Bits_05 is mod 2 ** 5;
   for Bits_05'Size use 5;

   package Indexing is new Packed_Arrays.Indexing (Bits_05);

   --  required for accessing arrays by compiler
   function Get_05 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_05
      renames Indexing.Get;
   procedure Set_05 (
      Arr : Address;
      N : Natural;
      E : Bits_05;
      Rev_SSO : Boolean)
      renames Indexing.Set;

end System.Pack_05;
