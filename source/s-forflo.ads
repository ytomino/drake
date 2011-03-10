pragma License (Unrestricted);
--  implementation package
package System.Formatting.Float is
   pragma Pure;

   subtype Longest_Unsigned_Float is Long_Long_Float
      range 0.0 .. Long_Long_Float'Last;

   --  decimal part for floating-point format = Aft / Base ** Exponent

   procedure Split (
      X : Longest_Unsigned_Float;
      Fore : out Unsigned; -- Fore < Base
      Aft : out Longest_Unsigned_Float;
      Exponent : out Integer;
      Base : Number_Base := 10);

   procedure Aft_Image (
      Value : Longest_Unsigned_Float;
      Exponent : Integer;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := Standard.Float'Digits - 1);
   --  Aft_Image puts '.', Item is required Width + 1

end System.Formatting.Float;
