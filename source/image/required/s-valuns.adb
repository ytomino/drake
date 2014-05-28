with System.Formatting.Literals;
with System.Value_Error;
package body System.Val_Uns is
   pragma Suppress (All_Checks);

   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned is
      Last : Natural;
      Result : Unsigned_Types.Unsigned;
      Error : Boolean;
   begin
      System.Formatting.Literals.Get_Literal (
         Str,
         Last,
         Formatting.Unsigned (Result),
         Error => Error);
      if not Error then
         System.Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Error ("Unsigned", Str);
   end Value_Unsigned;

end System.Val_Uns;
