package body System.Val_LLU is

   function Value_Long_Long_Unsigned (Str : String)
      return Unsigned_Types.Long_Long_Unsigned is
   begin
      raise Program_Error;
      return Value_Long_Long_Unsigned (Str);
   end Value_Long_Long_Unsigned;

end System.Val_LLU;
