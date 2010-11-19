package body System.Val_LLI is
   pragma Suppress (All_Checks);

   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer is
   begin
      raise Program_Error;
      return Value_Long_Long_Integer (Str);
   end Value_Long_Long_Integer;

end System.Val_LLI;
