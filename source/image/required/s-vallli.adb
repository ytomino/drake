with System.Formatting.Literals;
with System.Long_Long_Integer_Types;
with System.Value_Errors;
package body System.Val_LLI is

   subtype Word_Integer is Long_Long_Integer_Types.Word_Integer;

   --  implementation

   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer is
      Last : Natural;
      Result : Long_Long_Integer;
      Error : Boolean;
   begin
      if Long_Long_Integer'Size <= Standard'Word_Size then
         Formatting.Literals.Get_Literal (
            Str,
            Last,
            Word_Integer (Result),
            Error => Error);
      else
         Formatting.Literals.Get_Literal (Str, Last, Result, Error => Error);
      end if;
      if not Error then
         Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Errors.Raise_Discrete_Value_Failure ("Long_Long_Integer", Str);
      declare
         Uninitialized : Long_Long_Integer;
         pragma Unmodified (Uninitialized);
      begin
         return Uninitialized;
      end;
   end Value_Long_Long_Integer;

end System.Val_LLI;
