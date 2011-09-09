pragma License (Unrestricted);
--  implementation package required by compiler
package System.Compare_Array_Unsigned_8 is
   pragma Pure;

   --  required to compare arrays by compiler (s-carun8.ads)
   function Compare_Array_U8_Unaligned (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer;

   --  required to compare arrays by compiler (s-carun8.ads)
   function Compare_Array_U8 (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer
      renames Compare_Array_U8_Unaligned;

end System.Compare_Array_Unsigned_8;
