package body System.Val_WChar is

   function Value_Wide_Character (Str : String; EM : WC_Encoding_Method)
      return Wide_Character is
   begin
      raise Program_Error;
      return Value_Wide_Character (Str, EM);
   end Value_Wide_Character;

   function Value_Wide_Wide_Character (Str : String; EM : WC_Encoding_Method)
      return Wide_Wide_Character is
   begin
      raise Program_Error;
      return Value_Wide_Wide_Character (Str, EM);
   end Value_Wide_Wide_Character;

end System.Val_WChar;
