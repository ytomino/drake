package body System.Val_Dec is
   pragma Suppress (All_Checks);

   function Value_Decimal (Str : String; Scale : Integer) return Integer is
   begin
      raise Program_Error;
      return Value_Decimal (Str, Scale);
   end Value_Decimal;

end System.Val_Dec;
