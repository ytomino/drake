pragma License (Unrestricted);
--  implementation unit
package System.Formatting.Float is
   pragma Pure;

   subtype Longest_Float is Long_Long_Float;
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
      Set : Type_Set := Upper_Case;
      Width : Positive := Standard.Float'Digits - 1);
   --  Aft_Image puts '.', Item is required Width + 1

   function Fore_Width (Value : Longest_Float; Base : Number_Base := 10)
      return Positive;
   --  return width of integer part

end System.Formatting.Float;
