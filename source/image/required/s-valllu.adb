with System.Formatting.Literals;
with System.Long_Long_Integer_Types;
with System.Value_Errors;
package body System.Val_LLU is

   subtype Long_Long_Unsigned is Long_Long_Integer_Types.Long_Long_Unsigned;

   --  implementation

   function Value_Long_Long_Unsigned (Str : String)
      return Unsigned_Types.Long_Long_Unsigned
   is
      Last : Natural;
      Result : Unsigned_Types.Long_Long_Unsigned;
      Error : Boolean;
   begin
      Formatting.Literals.Get_Literal (
         Str,
         Last,
         Long_Long_Unsigned (Result),
         Error => Error);
      if not Error then
         Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Errors.Raise_Discrete_Value_Failure ("Long_Long_Unsigned", Str);
      declare
         Uninitialized : Unsigned_Types.Long_Long_Unsigned;
         pragma Unmodified (Uninitialized);
      begin
         return Uninitialized;
      end;
   end Value_Long_Long_Unsigned;

end System.Val_LLU;
