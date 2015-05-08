with System.Formatting.Literals;
with System.Value_Errors;
package body System.Val_Uns is
   pragma Suppress (All_Checks);

   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned is
      Last : Natural;
      Result : Unsigned_Types.Unsigned;
      Error : Boolean;
   begin
      Formatting.Literals.Get_Literal (
         Str,
         Last,
         Formatting.Unsigned (Result),
         Error => Error);
      if not Error then
         Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Errors.Raise_Discrete_Value_Failure ("Unsigned", Str);
      declare
         Uninitialized : Unsigned_Types.Unsigned;
         pragma Unmodified (Uninitialized);
      begin
         return Uninitialized;
      end;
   end Value_Unsigned;

end System.Val_Uns;
