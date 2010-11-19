package body System.Val_Bool is
   pragma Suppress (All_Checks);

   function Value_Boolean (Str : String) return Boolean is
   begin
      raise Program_Error;
      return Value_Boolean (Str);
   end Value_Boolean;

end System.Val_Bool;
