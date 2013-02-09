pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Compare_Array_Signed_8 is
   pragma Pure;

   --  required to compare arrays by compiler (s-carsi8.ads)
   function Compare_Array_S8_Unaligned (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;

   --  required to compare arrays by compiler (s-carsi8.ads)
   function Compare_Array_S8 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Compare_Array_S8_Unaligned;

end System.Compare_Array_Signed_8;
