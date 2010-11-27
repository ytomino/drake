with System.Val_Real;
package body System.Val_Dec is
   pragma Suppress (All_Checks);

   function Value_Decimal (Str : String; Scale : Integer) return Integer is
   begin
      return Integer (Val_Real.Value_Real (Str) * 10.0 ** Scale);
   end Value_Decimal;

end System.Val_Dec;
