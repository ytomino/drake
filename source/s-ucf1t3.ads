pragma License (Unrestricted);
--  implementation unit
package System.UTF_Conversions.From_16_To_32 is
   pragma Pure;

   procedure Convert is
      new Convert_Procedure (
         Wide_Character,
         Wide_String,
         Wide_Wide_Character,
         Wide_Wide_String,
         From_UTF_16,
         To_UTF_32);

   function Convert is
      new Convert_Function (
         Wide_Character,
         Wide_String,
         Wide_Wide_Character,
         Wide_Wide_String,
         Expanding_From_16_To_32,
         Convert);

end System.UTF_Conversions.From_16_To_32;
