with System.Val_Real;
package body System.Val_LLD is
   pragma Suppress (All_Checks);

   function Value_Long_Long_Decimal (Str : String; Scale : Integer)
      return Long_Long_Integer is
   begin
      return Long_Long_Integer (Val_Real.Value_Real (Str) * 10.0 ** Scale);
   end Value_Long_Long_Decimal;

end System.Val_LLD;
