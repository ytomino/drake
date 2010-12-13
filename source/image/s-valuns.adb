package body System.Val_Uns is
   pragma Suppress (All_Checks);
   use type Unsigned_Types.Unsigned;

   function Value_Unsigned (Str : String) return Unsigned_Types.Unsigned is
      Last : Natural;
      Result : Unsigned_Types.Unsigned;
   begin
      Get_Unsigned_Literal (Str, Last, Result);
      Check_Last (Str, Last);
      return Result;
   end Value_Unsigned;

   procedure Skip_Spaces (S : String; Last : in out Natural) is
   begin
      while Last < S'Last and then S (Last + 1) = ' ' loop
         Last := Last + 1;
      end loop;
   end Skip_Spaces;

   procedure Check_Last (S : String; Last : Natural) is
      I : Natural := Last;
   begin
      Skip_Spaces (S, I);
      if I /= S'Last then
         raise Constraint_Error;
      end if;
   end Check_Last;

   procedure Get_Unsigned (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Unsigned;
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
   end Get_Unsigned;

   procedure Get_Exponent (
      S : String;
      Last : in out Natural;
      Result : out Integer;
      Positive_Only : Boolean) is
   begin
      if Last < S'Last
         and then (S (Last + 1) = 'E' or else S (Last + 1) = 'e')
      then
         Last := Last + 1;
         if Last < S'Last and then S (Last + 1) = '-' then
            if Positive_Only then
               raise Constraint_Error;
            end if;
            Last := Last + 1;
            Get_Unsigned (S, Last, Formatting.Unsigned (Result), Base => 10);
            Result := -Result;
         else
            if Last < S'Last and then S (Last + 1) = '+' then
               Last := Last + 1;
            end if;
            Get_Unsigned (S, Last, Formatting.Unsigned (Result), Base => 10);
         end if;
      else
         Result := 0;
      end if;
   end Get_Exponent;

   procedure Get_Unsigned_Literal_Without_Sign (
      S : String;
      Last : in out Natural;
      Result : out Formatting.Unsigned)
   is
      Base : Formatting.Number_Base := 10;
      Mark : Character;
      Exponent : Integer;
   begin
      Get_Unsigned (S, Last, Result, Base => Base);
      if Last < S'Last
         and then (S (Last + 1) = '#' or else S (Last + 1) = ':')
      then
         Mark := S (Last + 1);
         Last := Last + 1;
         if Result < Formatting.Unsigned (Formatting.Number_Base'First)
            or else Result > Formatting.Unsigned (Formatting.Number_Base'Last)
         then
            raise Constraint_Error;
         end if;
         Base := Formatting.Number_Base (Result);
         Get_Unsigned (S, Last, Result, Base => Base);
         if Last >= S'Last or else S (Last + 1) /= Mark then
            raise Constraint_Error;
         end if;
         Last := Last + 1;
      end if;
      Get_Exponent (S, Last, Exponent, Positive_Only => True);
      if Exponent /= 0 then
         Result := Result * Formatting.Unsigned (Base) ** Exponent;
      end if;
   end Get_Unsigned_Literal_Without_Sign;

   procedure Get_Unsigned_Literal (
      S : String;
      Last : out Natural;
      Result : out Formatting.Unsigned) is
   begin
      Last := S'First - 1;
      Skip_Spaces (S, Last);
      if Last < S'Last and then S (Last + 1) = '+' then
         Last := Last + 1;
      end if;
      Get_Unsigned_Literal_Without_Sign (S, Last, Result);
   end Get_Unsigned_Literal;

end System.Val_Uns;
