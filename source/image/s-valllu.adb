with System.Val_Uns;
package body System.Val_LLU is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Long_Long_Unsigned;

   function Value_Long_Long_Unsigned (Str : String)
      return Unsigned_Types.Long_Long_Unsigned
   is
      Last : Natural;
      Result : Unsigned_Types.Long_Long_Unsigned;
   begin
      Get_Longest_Unsigned_Literal (Str, Last, Result);
      Val_Uns.Check_Last (Str, Last);
      return Result;
   end Value_Long_Long_Unsigned;

   procedure Get_Longest_Unsigned (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Longest_Unsigned;
      Base : Formatting.Number_Base)
   is
      Error : Boolean;
   begin
      Formatting.Value (
         S (Last + 1 .. S'Last),
         Last,
         Result,
         Base => Base,
         Skip_Underscore => True,
         Error => Error);
      if Error then
         raise Constraint_Error;
      end if;
   end Get_Longest_Unsigned;

   procedure Get_Longest_Unsigned_Literal_Without_Sign (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Longest_Unsigned)
   is
      Base : Formatting.Number_Base := 10;
      Mark : Character;
      Exponent : Integer;
   begin
      Get_Longest_Unsigned (S, Last, Result, Base => Base);
      if Last < S'Last
         and then (S (Last + 1) = '#' or else S (Last + 1) = ':')
      then
         Mark := S (Last + 1);
         Last := Last + 1;
         if Result < Formatting.Longest_Unsigned (Formatting.Number_Base'First)
            or else Result >
               Formatting.Longest_Unsigned (Formatting.Number_Base'Last)
         then
            raise Constraint_Error;
         end if;
         Base := Formatting.Number_Base (Result);
         Get_Longest_Unsigned (S, Last, Result, Base => Base);
         if Last >= S'Last or else S (Last + 1) /= Mark then
            raise Constraint_Error;
         end if;
         Last := Last + 1;
      end if;
      Val_Uns.Get_Exponent (S, Last, Exponent, Positive_Only => True);
      if Exponent /= 0 then
         Result := Result *
            System.Formatting.Longest_Unsigned (Base) ** Exponent;
      end if;
   end Get_Longest_Unsigned_Literal_Without_Sign;

   procedure Get_Longest_Unsigned_Literal (
      S : String;
      Last : out Natural;
      Result : out Formatting.Longest_Unsigned) is
   begin
      Last := S'First - 1;
      Val_Uns.Skip_Spaces (S, Last);
      if Last < S'Last and then S (Last + 1) = '+' then
         Last := Last + 1;
      end if;
      Get_Longest_Unsigned_Literal_Without_Sign (S, Last, Result);
   end Get_Longest_Unsigned_Literal;

end System.Val_LLU;
