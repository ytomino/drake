with System.Val_Real;
with System.Val_Uns;
with System.Value_Error;
package body System.Val_LLD is
   pragma Suppress (All_Checks);

   function Value_Long_Long_Decimal (Str : String; Scale : Integer)
      return Long_Long_Integer
   is
      Last : Natural := Str'First - 1;
      Result : Long_Long_Float;
      Error : Boolean;
   begin
      Val_Real.Get_Float_Literal (Str, Last, Result, Error);
      if not Error then
         Val_Uns.Check_Last (Str, Last, Error);
         if not Error then
            return Long_Long_Integer (Result * 10.0 ** Scale);
         end if;
      end if;
      Value_Error ("Long_Long_Decimal", Str);
   end Value_Long_Long_Decimal;

end System.Val_LLD;
