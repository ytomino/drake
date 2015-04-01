pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_15 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Bits_15 is mod 2 ** 15;
   for Bits_15'Size use 15;

   package Indexing is new Packed_Arrays.Indexing (Bits_15);

   --  required for accessing arrays by compiler
   function Get_15 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_15
      renames Indexing.Get;
   procedure Set_15 (
      Arr : Address;
      N : Natural;
      E : Bits_15;
      Rev_SSO : Boolean)
      renames Indexing.Set;

end System.Pack_15;
