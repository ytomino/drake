with System.Formatting;
with System.Val_Uns;
package body System.Val_Int is
   pragma Suppress (All_Checks);
   use type Formatting.Unsigned;

   function Value_Integer (Str : String) return Integer is
      Last : Natural;
      Result : Integer;
   begin
      Get_Integer_Literal (Str, Last, Result);
      Val_Uns.Check_Last (Str, Last);
      return Result;
   end Value_Integer;

   procedure Get_Integer_Literal (
      S : String;
      Last : out Natural;
      Result : out Integer)
   is
      Unsigned_Result : Formatting.Unsigned;
   begin
      Last := S'First - 1;
      Val_Uns.Skip_Spaces (S, Last);
      if Last < S'Last and then S (Last + 1) = '-' then
         Last := Last + 1;
         Val_Uns.Get_Unsigned_Literal_Without_Sign (
            S,
            Last,
            Unsigned_Result);
         if Unsigned_Result > Formatting.Unsigned (-Integer'First) then
            raise Constraint_Error;
         end if;
         Result := -Integer (Unsigned_Result);
      else
         if Last < S'Last and then S (Last + 1) = '+' then
            Last := Last + 1;
         end if;
         Val_Uns.Get_Unsigned_Literal_Without_Sign (
            S,
            Last,
            Unsigned_Result);
         if Unsigned_Result > Formatting.Unsigned (Integer'Last) then
            raise Constraint_Error;
         end if;
         Result := Integer (Unsigned_Result);
      end if;
   end Get_Integer_Literal;

end System.Val_Int;
