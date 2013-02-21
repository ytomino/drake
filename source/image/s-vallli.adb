with System.Formatting;
with System.Val_Uns;
with System.Val_LLU;
with System.Value_Error;
package body System.Val_LLI is
   pragma Suppress (All_Checks);
   use type Formatting.Longest_Unsigned;

   function Value_Long_Long_Integer (Str : String) return Long_Long_Integer is
      Last : Natural;
      Result : Long_Long_Integer;
      Error : Boolean;
   begin
      Get_Long_Long_Integer_Literal (Str, Last, Result, Error);
      if not Error then
         Val_Uns.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Error ("Long_Long_Integer", Str);
   end Value_Long_Long_Integer;

   procedure Get_Long_Long_Integer_Literal (
      S : String;
      Last : out Natural;
      Result : out Long_Long_Integer;
      Error : out Boolean)
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
            Unsigned_Result,
            Error => Error);
         if not Error then
            if Unsigned_Result <=
               Formatting.Longest_Unsigned'Mod (-Long_Long_Integer'First)
            then
               Result := -Long_Long_Integer (Unsigned_Result);
            else
               Error := True;
            end if;
         end if;
      else
         if Last < S'Last and then S (Last + 1) = '+' then
            Last := Last + 1;
         end if;
         Val_LLU.Get_Longest_Unsigned_Literal_Without_Sign (
            S,
            Last,
            Unsigned_Result,
            Error => Error);
         if not Error then
            if Unsigned_Result <=
               Formatting.Longest_Unsigned (Long_Long_Integer'Last)
            then
               Result := Long_Long_Integer (Unsigned_Result);
            else
               Error := True;
            end if;
         end if;
      end if;
   end Get_Long_Long_Integer_Literal;

end System.Val_LLI;
