pragma License (Unrestricted);
--  implementation unit
package System.UTF_Conversions.From_32_To_8 is
   pragma Pure;

   procedure Convert is new Convert_Procedure (
      Wide_Wide_Character,
      Wide_Wide_String,
      Character,
      String,
      From_UTF_32,
      To_UTF_8);

   function Convert is new Convert_Function (
      Wide_Wide_Character,
      Wide_Wide_String,
      Character,
      String,
      UTF_8_Max_Length,
      Convert);

end System.UTF_Conversions.From_32_To_8;
