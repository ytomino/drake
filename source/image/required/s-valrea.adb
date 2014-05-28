with System.Formatting.Literals.Float;
with System.Value_Error;
package body System.Val_Real is
   pragma Suppress (All_Checks);

   function Value_Real (Str : String) return Long_Long_Float is
      Last : Natural := Str'First - 1;
      Result : Long_Long_Float;
      Error : Boolean;
   begin
      System.Formatting.Literals.Float.Get_Literal (Str, Last, Result, Error);
      if not Error then
         System.Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Error ("Float", Str);
   end Value_Real;

end System.Val_Real;
