with System.Fore;
with System.Formatting.Float;
with System.Long_Long_Float_Divide;
procedure System.Formatting.Fixed_Image (
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
   Aft_Width : Positive)
is
   function signbit (X : Long_Long_Float) return Integer;
   pragma Import (Intrinsic, signbit, "__builtin_signbitl");
   Fore : Long_Long_Float;
   Aft : Long_Long_Float;
   Required_Fore_Width : Positive;
   Error : Boolean;
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
   Aft := abs Item;
   Fore := Long_Long_Float'Truncation (Aft);
   Aft := Aft - Fore;
   Required_Fore_Width := System.Fore.Fore_Width (Fore, Base => Base);
   for I in Required_Fore_Width + 1 .. Fore_Width loop
      Last := Last + 1;
      pragma Assert (Last <= To'Last);
      To (Last) := Fore_Padding;
   end loop;
   pragma Assert (Last + Required_Fore_Width <= To'Last);
   for I in reverse Last + 1 .. Last + Required_Fore_Width loop
      declare
         Q : Long_Long_Float;
         R : Long_Long_Float;
      begin
         Long_Long_Float_Divide (Fore, Long_Long_Float (Base), Q, R);
         Image (
            Digit (R),
            To (I),
            Casing => Casing);
         Fore := Q;
      end;
   end loop;
   Last := Last + Required_Fore_Width;
   --  '.' and decimal part
   pragma Assert (Last + Aft_Width <= To'Last);
   Float.Aft_Image (
      Aft,
      0,
      To (Last + 1 .. To'Last),
      Last,
      Base => Base,
      Casing => Casing,
      Width => Aft_Width);
   --  closing '#'
   if Base_Form then
      Last := Last + 1;
      pragma Assert (Last <= To'Last);
      To (Last) := '#';
   end if;
end System.Formatting.Fixed_Image;
