with System.Formatting.Float;
procedure System.Formatting.Float_Image (
   Value : Long_Long_Float;
   Item : out String;
   Last : out Natural;
   Minus_Sign : Character := '-';
   Zero_Sign : Character := ' ';
   Plus_Sign : Character := ' ';
   Base : Number_Base := 10;
   Base_Form : Boolean := False;
   Set : Type_Set := Upper_Case;
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
   pragma Suppress (All_Checks);
   use type Unsigned;
   function isnan (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, isnan, "__builtin_isnanl");
   function isinf (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, isinf, "__builtin_isinfl");
   function signbit (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, signbit, "__builtin_signbitl");
begin
   Last := Item'First - 1;
   if signbit (Value) /= 0 then
      if Minus_Sign /= No_Sign then
         Last := Last + 1;
         pragma Assert (Last <= Item'Last);
         Item (Last) := Minus_Sign;
      end if;
   elsif Value > 0.0 then
      if Plus_Sign /= No_Sign then
         Last := Last + 1;
         pragma Assert (Last <= Item'Last);
         Item (Last) := Plus_Sign;
      end if;
   else
      if Zero_Sign /= No_Sign then
         Last := Last + 1;
         pragma Assert (Last <= Item'Last);
         Item (Last) := Zero_Sign;
      end if;
   end if;
   if isnan (Value) /= 0 then
      declare
         First : constant Positive := Last + 1;
      begin
         Last := Last + NaN'Length;
         pragma Assert (Last <= Item'Last);
         Item (First .. Last) := NaN;
      end;
   elsif isinf (Value) /= 0 then
      declare
         First : constant Positive := Last + 1;
      begin
         Last := Last + Infinity'Length;
         pragma Assert (Last <= Item'Last);
         Item (First .. Last) := Infinity;
      end;
   else
      declare
         Fore : Digit;
         Aft : Long_Long_Float;
         Exponent : Integer;
         Scaled_Aft : Long_Long_Float;
         Rouned_Up : Boolean;
         Error : Boolean;
      begin
         Float.Split (
            abs Value,
            Fore,
            Aft,
            Exponent,
            Base => Base);
         Float.Aft_Scale (
            Aft,
            Scaled_Aft,
            Exponent,
            Rouned_Up,
            Base => Base,
            Width => Aft_Width);
         if Rouned_Up then
            Fore := Fore + 1;
            Scaled_Aft := 0.0;
            if Fore >= Base then
               Fore := 1;
               Exponent := Exponent + 1;
            end if;
         end if;
         --  opening '#'
         if Base_Form then
            Image (
               Unsigned (Base),
               Item (Last + 1 .. Item'Last),
               Last,
               Error => Error);
            pragma Assert (not Error);
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := '#';
         end if;
         --  integer part
         for I in 2 .. Fore_Width loop
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := Fore_Padding;
         end loop;
         Last := Last + 1;
         pragma Assert (Last <= Item'Last);
         Image (
            Fore,
            Item (Last),
            Set => Set);
         --  '.' and decimal part
         pragma Assert (Last + 1 + Aft_Width <= Item'Last);
         Float.Aft_Image (
            Scaled_Aft,
            Item (Last + 1 .. Item'Last),
            Last,
            Base => Base,
            Width => Aft_Width);
         --  closing #
         if Base_Form then
            Last := Last + 1;
            pragma Assert (Last <= Item'Last);
            Item (Last) := '#';
         end if;
         --  exponent
         Last := Last + 1;
         pragma Assert (Last <= Item'Last);
         Item (Last) := Exponent_Mark;
         if Exponent < 0 then
            if Exponent_Minus_Sign /= No_Sign then
               Last := Last + 1;
               pragma Assert (Last <= Item'Last);
               Item (Last) := Exponent_Minus_Sign;
            end if;
         elsif Exponent > 0 then
            if Exponent_Plus_Sign /= No_Sign then
               Last := Last + 1;
               pragma Assert (Last <= Item'Last);
               Item (Last) := Exponent_Plus_Sign;
            end if;
         else
            if Exponent_Zero_Sign /= No_Sign then
               Last := Last + 1;
               pragma Assert (Last <= Item'Last);
               Item (Last) := Exponent_Zero_Sign;
            end if;
         end if;
         Image (
            Unsigned (abs Exponent),
            Item (Last + 1 .. Item'Last),
            Last,
            Width => Exponent_Width,
            Padding => Exponent_Padding,
            Error => Error);
         pragma Assert (not Error);
      end;
   end if;
end System.Formatting.Float_Image;
