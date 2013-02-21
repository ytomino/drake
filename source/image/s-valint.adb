with System.Formatting;
with System.Val_Uns;
with System.Value_Error;
package body System.Val_Int is
   pragma Suppress (All_Checks);
   use type Formatting.Unsigned;

   function Value_Integer (Str : String) return Integer is
      Last : Natural;
      Result : Integer;
      Error : Boolean;
   begin
      Get_Integer_Literal (Str, Last, Result, Error);
      if not Error then
         Val_Uns.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Error ("Integer", Str);
   end Value_Integer;

   procedure Get_Integer_Literal (
      S : String;
      Last : out Natural;
      Result : out Integer;
      Error : out Boolean)
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
            Unsigned_Result,
            Error => Error);
         if not Error then
            if Unsigned_Result <= Formatting.Unsigned'Mod (-Integer'First) then
               Result := -Integer (Unsigned_Result);
            else
               Error := True;
            end if;
         end if;
      else
         if Last < S'Last and then S (Last + 1) = '+' then
            Last := Last + 1;
         end if;
         Val_Uns.Get_Unsigned_Literal_Without_Sign (
            S,
            Last,
            Unsigned_Result,
            Error => Error);
         if not Error then
            if Unsigned_Result <= Formatting.Unsigned (Integer'Last) then
               Result := Integer (Unsigned_Result);
            else
               Error := True;
            end if;
         end if;
      end if;
   end Get_Integer_Literal;

end System.Val_Int;
