pragma License (Unrestricted);
--  implementation unit
package System.Formatting.Float is
   pragma Pure;

   subtype Long_Long_Unsigned_Float is
      Long_Long_Float range 0.0 .. Long_Long_Float'Last;

   --  decimal part for floating-point format = Aft / Base ** Exponent

   procedure Aft_Scale (
      Aft : Long_Long_Unsigned_Float;
      Scaled_Aft : out Long_Long_Unsigned_Float;
      Exponent : Integer;
      Round_Up : out Boolean;
      Base : Number_Base := 10;
      Aft_Width : Positive := Standard.Float'Digits - 1);

   procedure Aft_Image (
      Value : Long_Long_Unsigned_Float; -- scaled Aft
      Item : out String; -- Item'Length >= Width + 1 for '.'
      Last : out Natural;
      Base : Number_Base := 10;
      Set : Type_Set := Upper_Case;
      Aft_Width : Positive := Standard.Float'Digits - 1);

   --  Width of integer part, without sign.
   function Fore_Digits_Width (
      Value : Long_Long_Unsigned_Float;
      Base : Number_Base := 10)
      return Positive;
   function Fore_Digits_Width (
      First, Last : Long_Long_Float;
      Base : Number_Base := 10)
      return Positive;

   procedure Image (
      Value : Long_Long_Float;
      Item : out String; -- Item'Length >= Long_Long_Float'Width + 4 for "16##"
      Fore_Last, Last : out Natural;
      Signs : Sign_Marks := ('-', ' ', ' ');
      Base : Number_Base := 10;
      Base_Form : Boolean := False;
      Set : Type_Set := Upper_Case;
      Fore_Digits_Width : Positive := 1;
      Fore_Digits_Fill : Character := '0';
      Aft_Width : Positive;
      Exponent_Mark : Character := 'E';
      Exponent_Signs : Sign_Marks := ('-', '+', '+');
      Exponent_Digits_Width : Positive := 2;
      Exponent_Digits_Fill : Character := '0';
      NaN : String := "NAN";
      Infinity : String := "INF");

end System.Formatting.Float;
