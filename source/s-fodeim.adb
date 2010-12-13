with System.Long_Long_Integer_Divide;
procedure System.Formatting.Decimal_Image (
   To : out String;
   Last : out Natural;
   Item : Long_Long_Integer;
   Scale : Integer;
   Minus_Sign : Character := '-';
   Zero_Sign : Character := ' ';
   Plus_Sign : Character := ' ';
   Fore_Width : Positive := 1;
   Fore_Padding : Character := '0';
   Aft_Width : Positive)
is
   use type Unsigned_Types.Long_Long_Unsigned;
   Error : Boolean;
begin
   Last := To'First - 1;
   if Item < 0 then
      if Minus_Sign /= Character'Val (0) then
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := Minus_Sign;
      end if;
   elsif Item > 0 then
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
   if Scale > 0 then
      declare
         Sp : constant Long_Long_Integer := 10 ** Scale;
         Q : Long_Long_Integer;
         R : Long_Long_Integer;
         Aft : System.Formatting.Longest_Unsigned;
         Error : Boolean;
      begin
         System.Long_Long_Integer_Divide (abs Item, Sp, Q, R);
         Aft := System.Formatting.Longest_Unsigned (R);
         System.Formatting.Image (
            System.Formatting.Longest_Unsigned (Q),
            To (Last + 1 .. To'Last),
            Last,
            Width => Fore_Width,
            Padding => Fore_Padding,
            Error => Error);
         pragma Assert (not Error);
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := '.';
         if Aft_Width > Scale then
            Aft := Aft * 10 ** (Aft_Width - Scale);
         elsif Aft_Width < Scale then
            Aft := Aft / 10 ** (Scale - Aft_Width);
         end if;
         System.Formatting.Image (
            Aft,
            To (Last + 1 .. To'Last),
            Last,
            Width => Aft_Width,
            Error => Error);
         pragma Assert (not Error);
      end;
   else
      if Item /= 0 then
         System.Formatting.Image (
            System.Formatting.Longest_Unsigned (abs Item),
            To (Last + 1 .. To'Last),
            Last,
            Width => Fore_Width,
            Padding => Fore_Padding,
            Error => Error);
         pragma Assert (not Error);
         for I in Scale .. -1 loop
            pragma Assert (Last + 1 <= To'Last);
            Last := Last + 1;
            To (Last) := '0';
         end loop;
      else
         for I in 2 .. Fore_Width loop
            Last := Last + 1;
            pragma Assert (Last <= To'Last);
            To (Last) := Fore_Padding;
         end loop;
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := '0';
      end if;
      Last := Last + 1;
      pragma Assert (Last <= To'Last);
      To (Last) := '.';
      for I in Last + 1 .. Last + Aft_Width loop
         Last := Last + 1;
         pragma Assert (Last <= To'Last);
         To (Last) := '0';
      end loop;
   end if;
end System.Formatting.Decimal_Image;
