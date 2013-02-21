with System.Value_Error;
package body System.Val_Uns is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Unsigned;

   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned is
      Last : Natural;
      Result : Unsigned_Types.Unsigned;
      Error : Boolean;
   begin
      Get_Unsigned_Literal (Str, Last, Result, Error);
      if not Error then
         Check_Last (Str, Last, Error);
         if not Error then
            return Result;
         end if;
      end if;
      Value_Error ("Unsigned", Str);
   end Value_Unsigned;

   procedure Skip_Spaces (S : String; Last : in out Natural) is
   begin
      while Last < S'Last and then S (Last + 1) = ' ' loop
         Last := Last + 1;
      end loop;
   end Skip_Spaces;

   procedure Check_Last (S : String; Last : Natural; Error : out Boolean) is
      I : Natural := Last;
   begin
      Skip_Spaces (S, I);
      Error := I /= S'Last;
   end Check_Last;

   procedure Get_Unsigned (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Unsigned;
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
   end Get_Unsigned;

   procedure Get_Exponent (
      S : String;
      Last : in out Natural;
      Result : out Integer;
      Positive_Only : Boolean;
      Error : out Boolean) is
   begin
      if Last < S'Last
         and then (S (Last + 1) = 'E' or else S (Last + 1) = 'e')
      then
         Last := Last + 1;
         if Last < S'Last and then S (Last + 1) = '-' then
            if not Positive_Only then
               Last := Last + 1;
               Get_Unsigned (S, Last, Formatting.Unsigned (Result),
                  Base => 10,
                  Error => Error);
               --  ignore error
               Result := -Result;
            else
               Error := True;
            end if;
         else
            if Last < S'Last and then S (Last + 1) = '+' then
               Last := Last + 1;
            end if;
            Get_Unsigned (S, Last, Formatting.Unsigned (Result),
               Base => 10,
               Error => Error);
         end if;
      else
         Result := 0;
         Error := False;
      end if;
   end Get_Exponent;

   procedure Get_Unsigned_Literal_Without_Sign (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Unsigned;
      Error : out Boolean)
   is
      Base : Formatting.Number_Base := 10;
      Mark : Character;
      Exponent : Integer;
   begin
      Get_Unsigned (S, Last, Result, Base => Base, Error => Error);
      if not Error then
         if Last < S'Last
            and then (S (Last + 1) = '#' or else S (Last + 1) = ':')
         then
            Mark := S (Last + 1);
            Last := Last + 1;
            if Result in
               Formatting.Unsigned (Formatting.Number_Base'First) ..
               Formatting.Unsigned (Formatting.Number_Base'Last)
            then
               Base := Formatting.Number_Base (Result);
               Get_Unsigned (S, Last, Result, Base => Base, Error => Error);
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
         Get_Exponent (S, Last, Exponent,
            Positive_Only => True,
            Error => Error);
         if not Error and then Exponent /= 0 then
            Result := Result * Formatting.Unsigned (Base) ** Exponent;
         end if;
      end if;
   end Get_Unsigned_Literal_Without_Sign;

   procedure Get_Unsigned_Literal (
      S : String;
      Last : out Natural;
      Result : out Formatting.Unsigned;
      Error : out Boolean) is
   begin
      Last := S'First - 1;
      Skip_Spaces (S, Last);
      if Last < S'Last and then S (Last + 1) = '+' then
         Last := Last + 1;
      end if;
      Get_Unsigned_Literal_Without_Sign (S, Last, Result, Error);
   end Get_Unsigned_Literal;

end System.Val_Uns;
