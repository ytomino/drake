pragma License (Unrestricted);
--  implementation package
package System.UTF_Conversions.From_32_To_16 is
   pragma Pure;

   procedure Convert is new Convert_Procedure (
      Wide_Wide_Character,
      Wide_Wide_String,
      Wide_Character,
      Wide_String,
      From_UTF_32,
      To_UTF_16);

   function Convert is new Convert_Function (
      Wide_Wide_Character,
      Wide_Wide_String,
      Wide_Character,
      Wide_String,
      UTF_16_Max_Length,
      Convert);

end System.UTF_Conversions.From_32_To_16;
