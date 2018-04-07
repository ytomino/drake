pragma License (Unrestricted);
--  implementation unit
package System.Formatting.Decimal is
   pragma Pure;

   procedure Image (
      Value : Long_Long_Integer;
      Item : out String; -- To'Length >= T'Fore + T'Aft + 1 (.)
      Fore_Last, Last : out Natural;
      Scale : Integer;
      Signs : Sign_Marks := ('-', ' ', ' ');
      Fore_Digits_Width : Positive := 1;
      Fore_Digits_Fill : Character := '0';
      Aft_Width : Natural);

end System.Formatting.Decimal;
