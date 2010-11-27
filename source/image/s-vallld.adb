package body System.Val_LLD is
   pragma Suppress (All_Checks);

   function Value_Long_Long_Decimal (Str : String; Scale : Integer)
      return Long_Long_Integer is
   begin
      raise Program_Error;
      return Value_Long_Long_Decimal (Str, Scale);
   end Value_Long_Long_Decimal;

end System.Val_LLD;
