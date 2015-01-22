pragma License (Unrestricted);
--  runtime unit
package System.Formatting is
   pragma Pure;

   subtype Number_Base is Integer range 2 .. 16; -- same as Text_IO.Number_Base
   subtype Digit is Integer range 0 .. 15;

   type Unsigned is mod 2 ** Integer'Size;
   type Longest_Unsigned is mod 2 ** Long_Long_Integer'Size;

   type Type_Set is (Lower_Case, Upper_Case); -- same as Text_IO.Type_Set
   pragma Discard_Names (Type_Set);

   function Width (Value : Unsigned; Base : Number_Base := 10) return Positive;
   function Width (Value : Longest_Unsigned; Base : Number_Base := 10)
      return Positive;

   procedure Image (
      Value : Digit;
      Item : out Character;
      Set : Type_Set := Upper_Case);

   procedure Image (
      Value : Unsigned;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Set : Type_Set := Upper_Case;
      Width : Positive := 1;
      Padding : Character := '0';
      Error : out Boolean);

   procedure Image (
      Value : Longest_Unsigned;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Set : Type_Set := Upper_Case;
      Width : Positive := 1;
      Padding : Character := '0';
      Error : out Boolean);

   procedure Value (
      Item : Character;
      Result : out Digit;
      Error : out Boolean);

   procedure Value (
      Item : String;
      Last : out Natural;
      Result : out Unsigned;
      Base : Number_Base := 10;
      Skip_Underscore : Boolean := False;
      Error : out Boolean);

   procedure Value (
      Item : String;
      Last : out Natural;
      Result : out Longest_Unsigned;
      Base : Number_Base := 10;
      Skip_Underscore : Boolean := False;
      Error : out Boolean);

   No_Sign : constant Character := Character'Val (16#ff#);

end System.Formatting;
