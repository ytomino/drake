pragma License (Unrestricted);
--  implementation unit
package System.Formatting.Fixed is
   pragma Pure;

   procedure Image (
      Value : Long_Long_Float;
      Item : out String; -- To'Length >= T'Fore + T'Aft + 5 (16#.#)
      Fore_Last, Last : out Natural;
      Signs : Sign_Marks := ('-', ' ', ' ');
      Base : Number_Base := 10;
      Base_Form : Boolean := False;
      Set : Type_Set := Upper_Case;
      Fore_Digits_Width : Positive := 1;
      Fore_Digits_Fill : Character := '0';
      Aft_Width : Positive);

end System.Formatting.Fixed;
