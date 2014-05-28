with System.Formatting.Literals;
with System.Value_Error;
package body System.Val_LLI is
   pragma Suppress (All_Checks);

   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer is
      Last : Natural;
      Result : Long_Long_Integer;
      Error : Boolean;
   begin
      Formatting.Literals.Get_Literal (Str, Last, Result, Error);
      if not Error then
         Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Error ("Long_Long_Integer", Str);
   end Value_Long_Long_Integer;

end System.Val_LLI;
