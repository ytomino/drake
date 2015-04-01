pragma License (Unrestricted);
--  implementation unit
package System.Packed_Arrays is
   pragma Pure;

   --  for System.Compare_Array_Signed_XX/Compare_Array_Unsigned_XX

   generic
      type Element_Type is (<>);
   package Ordering is
      --  this surrounding package is necessary for propagating
      --    pragma Machine_Attribute to its instances.

      function Compare (
         Left : Address;
         Right : Address;
         Left_Len : Natural;
         Right_Len : Natural)
         return Integer;
      pragma Machine_Attribute (Compare, "pure");

   end Ordering;

   --  for System.Pack_XX

   generic
      type Element_Type is (<>);
   package Indexing is

      function Get (
         Arr : Address;
         N : Natural;
         Rev_SSO : Boolean)
         return Element_Type
         with Convention => Intrinsic;

      procedure Set (
         Arr : Address;
         N : Natural;
         E : Element_Type;
         Rev_SSO : Boolean)
         with Convention => Intrinsic;

      pragma Machine_Attribute (Get, "pure");
      pragma Inline_Always (Get);
      pragma Inline_Always (Set);

   end Indexing;

end System.Packed_Arrays;
