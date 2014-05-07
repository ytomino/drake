pragma License (Unrestricted);
--  implementation unit
package System.UTF_Conversions.From_8_To_16 is
   pragma Pure;

   pragma Suppress (All_Checks); -- for instantiation

   procedure Convert is
      new Convert_Procedure (
         Character,
         String,
         Wide_Character,
         Wide_String,
         From_UTF_8,
         To_UTF_16);

   function Convert is
      new Convert_Function (
         Character,
         String,
         Wide_Character,
         Wide_String,
         Expanding_From_8_To_16,
         Convert);

end System.UTF_Conversions.From_8_To_16;
