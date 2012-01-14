pragma License (Unrestricted);
--  implementation unit
procedure System.Formatting.Decimal_Image (
   To : out String; -- To'Length >= T'Fore + T'Aft + 1 (.)
   Last : out Natural;
   Item : Long_Long_Integer;
   Scale : Integer;
   Minus_Sign : Character := '-';
   Zero_Sign : Character := ' ';
   Plus_Sign : Character := ' ';
   Fore_Width : Positive := 1;
   Fore_Padding : Character := '0';
   Aft_Width : Natural);
pragma Pure (System.Formatting.Decimal_Image);
