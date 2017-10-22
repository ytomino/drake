with System.Formatting.Literals;
with System.Long_Long_Integer_Types;
with System.Value_Errors;
package body System.Val_Int is

   subtype Word_Integer is Long_Long_Integer_Types.Word_Integer;

   --  implementation

   function Value_Integer (Str : String) return Integer is
      Last : Natural;
      Result : Word_Integer;
      Error : Boolean;
   begin
      Formatting.Literals.Get_Literal (Str, Last, Result, Error);
      if not Error
         and then (
            Standard'Word_Size = Integer'Size
            or else Result in
               Word_Integer (Integer'First) .. Word_Integer (Integer'Last))
      then
         Formatting.Literals.Check_Last (Str, Last, Error);
         if not Error then
            return Integer (Result);
         end if;
      end if;
      Value_Errors.Raise_Discrete_Value_Failure ("Integer", Str);
      declare
         Uninitialized : Integer;
         pragma Unmodified (Uninitialized);
      begin
         return Uninitialized;
      end;
   end Value_Integer;

end System.Val_Int;
