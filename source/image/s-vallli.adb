with System.Unsigned_Types;
with System.Val_Uns;
with System.Val_LLU;
package body System.Val_LLI is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Long_Long_Unsigned;

   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer is
      Index : Positive := Str'First;
      Unsigned_Result : Unsigned_Types.Long_Long_Unsigned;
      Result : Long_Long_Integer;
   begin
      Val_Uns.Skip_Spaces (Str, Index);
      if Index <= Str'Last and then Str (Index) = '-' then
         Index := Index + 1;
         Val_LLU.Get_Longest_Unsigned_Literal_Without_Sign (
            Str,
            Index,
            Unsigned_Result);
         if Unsigned_Result >
            Unsigned_Types.Long_Long_Unsigned (-Long_Long_Integer'First)
         then
            raise Constraint_Error;
         end if;
         Result := -Long_Long_Integer (Unsigned_Result);
      else
         if Index <= Str'Last and then Str (Index) = '+' then
            Index := Index + 1;
         end if;
         Val_LLU.Get_Longest_Unsigned_Literal_Without_Sign (
            Str,
            Index,
            Unsigned_Result);
         if Unsigned_Result >
            Unsigned_Types.Long_Long_Unsigned (Long_Long_Integer'Last)
         then
            raise Constraint_Error;
         end if;
         Result := Long_Long_Integer (Unsigned_Result);
      end if;
      Val_Uns.Check_Last (Str, Index);
      return Result;
   end Value_Long_Long_Integer;

end System.Val_LLI;
