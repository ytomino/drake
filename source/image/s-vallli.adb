with System.Formatting;
with System.Val_Uns;
with System.Val_LLU;
package body System.Val_LLI is
   pragma Suppress (All_Checks);
   use type Formatting.Longest_Unsigned;

   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer is
      Last : Natural;
      Result : Long_Long_Integer;
   begin
      Get_Long_Long_Integer_Literal (Str, Last, Result);
      Val_Uns.Check_Last (Str, Last);
      return Result;
   end Value_Long_Long_Integer;

   procedure Get_Long_Long_Integer_Literal (
      S : String;
      Last : out Natural;
      Result : out Long_Long_Integer)
   is
      Unsigned_Result : Formatting.Longest_Unsigned;
   begin
      Last := S'First - 1;
      Val_Uns.Skip_Spaces (S, Last);
      if Last < S'Last and then S (Last + 1) = '-' then
         Last := Last + 1;
         Val_LLU.Get_Longest_Unsigned_Literal_Without_Sign (
            S,
            Last,
            Unsigned_Result);
         if Unsigned_Result >
            Formatting.Longest_Unsigned (-Long_Long_Integer'First)
         then
            raise Constraint_Error;
         end if;
         Result := -Long_Long_Integer (Unsigned_Result);
      else
         if Last < S'Last and then S (Last + 1) = '+' then
            Last := Last + 1;
         end if;
         Val_LLU.Get_Longest_Unsigned_Literal_Without_Sign (
            S,
            Last,
            Unsigned_Result);
         if Unsigned_Result >
            Formatting.Longest_Unsigned (Long_Long_Integer'Last)
         then
            raise Constraint_Error;
         end if;
         Result := Long_Long_Integer (Unsigned_Result);
      end if;
   end Get_Long_Long_Integer_Literal;

end System.Val_LLI;
