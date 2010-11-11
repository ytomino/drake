package body System.Val_Char is

   function Value_Character (Str : String) return Character is
   begin
      raise Program_Error;
      return Value_Character (Str);
   end Value_Character;

end System.Val_Char;
