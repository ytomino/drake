package body System.Val_Real is

   function Value_Real (Str : String) return Long_Long_Float is
   begin
      raise Program_Error;
      return Value_Real (Str);
   end Value_Real;

end System.Val_Real;
