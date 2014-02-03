with System.Val_Uns;
with System.Value_Error;
package body System.Val_LLU is
   pragma Suppress (All_Checks);
   use type Formatting.Longest_Unsigned;
   use type Unsigned_Types.Long_Long_Unsigned;

   function Value_Long_Long_Unsigned (Str : String)
      return Unsigned_Types.Long_Long_Unsigned
   is
      Last : Natural;
      Result : Unsigned_Types.Long_Long_Unsigned;
      Error : Boolean;
   begin
      Get_Longest_Unsigned_Literal (
         Str,
         Last,
         Formatting.Longest_Unsigned (Result),
         Error => Error);
      if not Error then
         Val_Uns.Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Error ("Long_Long_Unsigned", Str);
   end Value_Long_Long_Unsigned;

   procedure Get_Longest_Unsigned (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Longest_Unsigned;
      Base : Formatting.Number_Base;
      Error : out Boolean) is
   begin
      Formatting.Value (
         S (Last + 1 .. S'Last),
         Last,
         Result,
         Base => Base,
         Skip_Underscore => True,
         Error => Error);
   end Get_Longest_Unsigned;

   procedure Get_Longest_Unsigned_Literal_Without_Sign (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Longest_Unsigned;
      Error : out Boolean)
   is
      Base : Formatting.Number_Base := 10;
      Mark : Character;
      Exponent : Integer;
   begin
      Get_Longest_Unsigned (S, Last, Result, Base => Base, Error => Error);
      if not Error then
         if Last < S'Last
            and then (S (Last + 1) = '#' or else S (Last + 1) = ':')
         then
            Mark := S (Last + 1);
            Last := Last + 1;
            if Result in
               Formatting.Longest_Unsigned (Formatting.Number_Base'First) ..
               Formatting.Longest_Unsigned (Formatting.Number_Base'Last)
            then
               Base := Formatting.Number_Base (Result);
               Get_Longest_Unsigned (S, Last, Result,
                  Base => Base,
                  Error => Error);
               if not Error then
                  if Last < S'Last and then S (Last + 1) = Mark then
                     Last := Last + 1;
                  else
                     Error := True;
                     return;
                  end if;
               else
                  return;
               end if;
            else
               Error := True;
               return;
            end if;
         end if;
         Val_Uns.Get_Exponent (S, Last, Exponent,
            Positive_Only => True,
            Error => Error);
         if not Error and then Exponent /= 0 then
            Result := Result * Formatting.Longest_Unsigned (Base) ** Exponent;
         end if;
      end if;
   end Get_Longest_Unsigned_Literal_Without_Sign;

   procedure Get_Longest_Unsigned_Literal (
      S : String;
      Last : out Natural;
      Result : out Formatting.Longest_Unsigned;
      Error : out Boolean) is
   begin
      Last := S'First - 1;
      Val_Uns.Skip_Spaces (S, Last);
      if Last < S'Last and then S (Last + 1) = '+' then
         Last := Last + 1;
      end if;
      Get_Longest_Unsigned_Literal_Without_Sign (S, Last, Result, Error);
   end Get_Longest_Unsigned_Literal;

end System.Val_LLU;
