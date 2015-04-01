pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_13 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Bits_13 is mod 2 ** 13;
   for Bits_13'Size use 13;

   package Indexing is new Packed_Arrays.Indexing (Bits_13);

   --  required for accessing arrays by compiler
   function Get_13 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_13
      renames Indexing.Get;
   procedure Set_13 (
      Arr : Address;
      N : Natural;
      E : Bits_13;
      Rev_SSO : Boolean)
      renames Indexing.Set;

end System.Pack_13;
