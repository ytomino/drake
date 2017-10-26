with System.Formatting.Literals;
with System.Long_Long_Integer_Types;
with System.Value_Errors;
package body System.Val_Uns is
   use type Long_Long_Integer_Types.Word_Unsigned;

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   --  implementation

   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned is
      Last : Natural;
      Result : Word_Unsigned;
      Error : Boolean;
   begin
      Formatting.Literals.Get_Literal (Str, Last, Result, Error => Error);
      if not Error
         and then (
            Standard'Word_Size = Unsigned_Types.Unsigned'Size
            or else Result <= Word_Unsigned (Unsigned_Types.Unsigned'Last))
      then
         Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Unsigned_Types.Unsigned (Result);
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
