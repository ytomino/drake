pragma License (Unrestricted);
--  implementation unit
package System.UTF_Conversions.From_16_To_8 is
   pragma Pure;

   pragma Suppress (All_Checks); -- for instantiation

   procedure Convert is
      new Convert_Procedure (
         Wide_Character,
         Wide_String,
         Character,
         String,
         From_UTF_16,
         To_UTF_8);

   function Convert is
      new Convert_Function (
         Wide_Character,
         Wide_String,
         Character,
         String,
         Expanding_From_16_To_8,
         Convert);

end System.UTF_Conversions.From_16_To_8;
