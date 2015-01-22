pragma License (Unrestricted);
--  implementation unit
package System.Formatting.Decimal is
   pragma Pure;

   procedure Image (
      Value : Long_Long_Integer;
      Item : out String; -- To'Length >= T'Fore + T'Aft + 1 (.)
      Last : out Natural;
      Scale : Integer;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Fore_Width : Positive := 1;
      Fore_Padding : Character := '0';
      Aft_Width : Natural);

end System.Formatting.Decimal;
