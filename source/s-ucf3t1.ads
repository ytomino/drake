pragma License (Unrestricted);
--  implementation unit
package System.UTF_Conversions.From_32_To_16 is
   pragma Pure;

   pragma Suppress (All_Checks); -- for instantiation

   procedure Convert is
      new Convert_Procedure (
         Wide_Wide_Character,
         Wide_Wide_String,
         Wide_Character,
         Wide_String,
         From_UTF_32,
         To_UTF_16);

   function Convert is
      new Convert_Function (
         Wide_Wide_Character,
         Wide_Wide_String,
         Wide_Character,
         Wide_String,
         Expanding_From_32_To_16,
         Convert);

end System.UTF_Conversions.From_32_To_16;
