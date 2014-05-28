with System.Formatting.Literals;
with System.Value_Error;
package body System.Val_LLU is
   pragma Suppress (All_Checks);

   function Value_Long_Long_Unsigned (Str : String)
      return Unsigned_Types.Long_Long_Unsigned
   is
      Last : Natural;
      Result : Unsigned_Types.Long_Long_Unsigned;
      Error : Boolean;
   begin
      System.Formatting.Literals.Get_Literal (
         Str,
         Last,
         Formatting.Longest_Unsigned (Result),
         Error => Error);
      if not Error then
         System.Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Error ("Long_Long_Unsigned", Str);
   end Value_Long_Long_Unsigned;

end System.Val_LLU;
