pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Packed_Arrays;
package System.Pack_12 is
   pragma Preelaborate;
   --  if this is Pure, subprograms would become __attribute__((const)).

   type Bits_12 is mod 2 ** 12;
   for Bits_12'Size use 12;

   package Indexing is new Packed_Arrays.Indexing (Bits_12);

   --  required for accessing aligned arrays by compiler
   function Get_12 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_12
      renames Indexing.Get;
   procedure Set_12 (
      Arr : Address;
      N : Natural;
      E : Bits_12;
      Rev_SSO : Boolean)
      renames Indexing.Set;

   --  required for accessing unaligned arrays by compiler
   function GetU_12 (
      Arr : Address;
      N : Natural;
      Rev_SSO : Boolean)
      return Bits_12
      renames Indexing.Get;
   procedure SetU_12 (
      Arr : Address;
      N : Natural;
      E : Bits_12;
      Rev_SSO : Boolean)
      renames Indexing.Set;

end System.Pack_12;
