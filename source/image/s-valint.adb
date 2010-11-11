with System.Unsigned_Types;
with System.Val_Uns;
package body System.Val_Int is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Unsigned;

   function Value_Integer (Str : String) return Integer is
      Position : Positive := Str'First;
      Unsigned_Result : Unsigned_Types.Unsigned;
      Result : Integer;
   begin
      if Position > Str'Last then
         raise Constraint_Error;
      elsif Str (Position) = '-' then
         Position := Position + 1;
         Unsigned_Result := Val_Uns.Value_Unsigned (
            Str (Position .. Str'Last));
         if Unsigned_Result > Unsigned_Types.Unsigned (-Integer'First) then
            raise Constraint_Error;
         end if;
         Result := Integer (Unsigned_Result);
      else
         if Str (Position) = '+' then
            Position := Position + 1;
         end if;
         Unsigned_Result := Val_Uns.Value_Unsigned (
            Str (Position .. Str'Last));
         if Unsigned_Result > Unsigned_Types.Unsigned (Integer'Last) then
            raise Constraint_Error;
         else
            Result := Integer (Unsigned_Result);
         end if;
      end if;
      return Result;
   end Value_Integer;

end System.Val_Int;
