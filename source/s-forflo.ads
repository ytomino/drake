pragma License (Unrestricted);
--  implementation unit
package System.Formatting.Float is
   pragma Pure;

   subtype Longest_Float is Long_Long_Float;
   subtype Longest_Unsigned_Float is
      Long_Long_Float range 0.0 .. Long_Long_Float'Last;

   --  decimal part for floating-point format = Aft / Base ** Exponent

   procedure Aft_Scale (
      Aft : Longest_Unsigned_Float;
      Scaled_Aft : out Longest_Unsigned_Float;
      Exponent : Integer;
      Round_Up : out Boolean;
      Base : Number_Base := 10;
      Width : Positive := Standard.Float'Digits - 1);

   procedure Aft_Image (
      Value : Longest_Unsigned_Float; -- scaled Aft
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Set : Type_Set := Upper_Case;
      Width : Positive := Standard.Float'Digits - 1);
   --  Aft_Image puts '.', Item is required Width + 1

   function Fore_Width (Value : Longest_Float; Base : Number_Base := 10)
      return Positive;
   function Fore_Width (First, Last : Longest_Float; Base : Number_Base := 10)
      return Positive;
   pragma Inline (Fore_Width);
   --  return width of integer part

   procedure Image (
      Value : Longest_Float;
      Item : out String; -- To'Length >= Long_Long_Float'Width + 4 (16##)
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
      Infinity : String := "INF");

end System.Formatting.Float;
