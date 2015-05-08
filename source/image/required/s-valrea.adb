with System.Formatting.Literals.Float;
with System.Value_Errors;
package body System.Val_Real is
   pragma Suppress (All_Checks);

   function Value_Real (Str : String) return Long_Long_Float is
      Last : Natural := Str'First - 1;
      Result : Long_Long_Float;
      Error : Boolean;
   begin
      Formatting.Literals.Float.Get_Literal (Str, Last, Result, Error);
      if not Error then
         Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Errors.Raise_Value_Failure ("Float", Str);
   end Value_Real;

end System.Val_Real;
