with System.Unsigned_Types;
with System.Val_Uns;
package body System.Val_Int is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Unsigned;

   function Value_Integer (Str : String) return Integer is
      Index : Positive := Str'First;
      Unsigned_Result : Unsigned_Types.Unsigned;
      Result : Integer;
   begin
      Val_Uns.Skip_Spaces (Str, Index);
      if Index <= Str'Last and then Str (Index) = '-' then
         Index := Index + 1;
         Val_Uns.Get_Unsigned_Literal_Without_Sign (
            Str,
            Index,
            Unsigned_Result);
         if Unsigned_Result > Unsigned_Types.Unsigned (-Integer'First) then
            raise Constraint_Error;
         end if;
         Result := -Integer (Unsigned_Result);
      else
         if Index <= Str'Last and then Str (Index) = '+' then
            Index := Index + 1;
         end if;
         Val_Uns.Get_Unsigned_Literal_Without_Sign (
            Str,
            Index,
            Unsigned_Result);
         if Unsigned_Result > Unsigned_Types.Unsigned (Integer'Last) then
            raise Constraint_Error;
         end if;
         Result := Integer (Unsigned_Result);
      end if;
      Val_Uns.Check_Last (Str, Index);
      return Result;
   end Value_Integer;

end System.Val_Int;
