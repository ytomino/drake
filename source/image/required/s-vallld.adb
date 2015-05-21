with System.Formatting.Literals.Float;
with System.Value_Errors;
package body System.Val_LLD is
   pragma Suppress (All_Checks);

   function Value_Long_Long_Decimal (Str : String; Scale : Integer)
      return Long_Long_Integer
   is
      Last : Natural := Str'First - 1;
      Result : Long_Long_Float;
      Error : Boolean;
   begin
      Formatting.Literals.Float.Get_Literal (Str, Last, Result, Error);
      if not Error then
         Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Long_Long_Integer (Result * 10.0 ** Scale);
         end if;
      end if;
      Value_Errors.Raise_Value_Failure ("Long_Long_Decimal", Str);
   end Value_Long_Long_Decimal;

end System.Val_LLD;
