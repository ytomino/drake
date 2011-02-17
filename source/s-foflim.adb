with System.Formatting.Float;
procedure System.Formatting.Float_Image (
   To : out String;
   Last : out Natural;
   Item : Long_Long_Float;
   Minus_Sign : Character := '-';
   Zero_Sign : Character := ' ';
   Plus_Sign : Character := ' ';
   Base : Number_Base := 10;
   Base_Form : Boolean := False;
   Casing : Casing_Type := Upper;
   Fore_Width : Positive := 1;
   Fore_Padding : Character := '0';
   Aft_Width : Positive;
   Exponent_Mark : Character := 'E';
   Exponent_Minus_Sign : Character := '-';
   Exponent_Zero_Sign : Character := '+';
   Exponent_Plus_Sign : Character := '+';
   Exponent_Width : Positive := 2;
   Exponent_Padding : Character := '0';
   NaN : String := "NAN";
   Infinity : String := "INF")
is
   function isnan (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, isnan, "__builtin_isnanl");
   function isinf (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, isinf, "__builtin_isinfl");
   function signbit (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, signbit, "__builtin_signbitl");
begin
   Last := To'First - 1;
   if signbit (Item) /= 0 then
      if Minus_Sign /= Character'Val (0) then
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := Minus_Sign;
      end if;
   elsif Item > 0.0 then
      if Plus_Sign /= Character'Val (0) then
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := Plus_Sign;
      end if;
   else
      if Zero_Sign /= Character'Val (0) then
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := Zero_Sign;
      end if;
   end if;
   if isnan (Item) /= 0 then
      declare
         First : constant Positive := Last + 1;
      begin
         Last := Last + NaN'Length;
         pragma Assert (Last <= To'Last);
         To (First .. Last) := NaN;
      end;
   elsif isinf (Item) /= 0 then
      declare
         First : constant Positive := Last + 1;
      begin
         Last := Last + Infinity'Length;
         pragma Assert (Last <= To'Last);
         To (First .. Last) := Infinity;
      end;
   else
      declare
         Fore : Unsigned;
         Aft : Long_Long_Float;
         Exponent : Integer;
         Error : Boolean;
      begin
         Float.Split (
            abs Item,
            Fore,
            Aft,
            Exponent,
            Base => Base);
         --  opening '#'
         if Base_Form then
            Image (
               Unsigned (Base),
               To (Last + 1 .. To'Last),
               Last,
               Error => Error);
            pragma Assert (not Error);
            Last := Last + 1;
            pragma Assert (Last <= To'Last);
            To (Last) := '#';
         end if;
         --  integer part
         for I in 2 .. Fore_Width loop
            Last := Last + 1;
            pragma Assert (Last <= To'Last);
            To (Last) := Fore_Padding;
         end loop;
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         Image (
            Fore,
            To (Last),
            Casing => Casing);
         --  '.' and decimal part
         pragma Assert (Last + 1 + Aft_Width <= To'Last);
         Float.Aft_Image (
            Aft,
            To (Last + 1 .. To'Last),
            Last,
            Base => Base,
            Width => Aft_Width);
         --  closing #
         if Base_Form then
            Last := Last + 1;
            pragma Assert (Last <= To'Last);
            To (Last) := '#';
         end if;
         --  exponent
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := Exponent_Mark;
         if Exponent < 0 then
            if Exponent_Minus_Sign /= Character'Val (0) then
               Last := Last + 1;
               pragma Assert (Last <= To'Last);
               To (Last) := Exponent_Minus_Sign;
            end if;
         elsif Exponent > 0 then
            if Exponent_Plus_Sign /= Character'Val (0) then
               Last := Last + 1;
               pragma Assert (Last <= To'Last);
               To (Last) := Exponent_Plus_Sign;
            end if;
         else
            if Exponent_Zero_Sign /= Character'Val (0) then
               Last := Last + 1;
               pragma Assert (Last <= To'Last);
               To (Last) := Exponent_Zero_Sign;
            end if;
         end if;
         Image (
            Unsigned (abs Exponent),
            To (Last + 1 .. To'Last),
            Last,
            Width => Exponent_Width,
            Padding => Exponent_Padding,
            Error => Error);
         pragma Assert (not Error);
      end;
   end if;
end System.Formatting.Float_Image;
