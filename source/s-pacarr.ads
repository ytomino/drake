pragma License (Unrestricted);
--  implementation unit
package System.Packed_Arrays is
   pragma Pure;

   --  for System.Compare_Array_Signed_XX/Compare_Array_Unsigned_XX

   generic
      type Element_Type is (<>);
   function Compare (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;
   pragma Pure_Function (Compare);

   --  for System.Pack_XX

   generic
      type Element_Type is (<>);
   package Indexing is

      function Get (Arr : Address; N : Natural) return Element_Type;
      pragma Pure_Function (Get);
      pragma Inline (Get);

      procedure Set (Arr : Address; N : Natural; E : Element_Type);
      pragma Inline (Set);

   end Indexing;

end System.Packed_Arrays;
