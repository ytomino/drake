pragma License (Unrestricted);
--  runtime unit
with System.Long_Long_Integer_Types;
package System.Formatting is
   pragma Pure;

   subtype Number_Base is Integer range 2 .. 16; -- same as Text_IO.Number_Base
   subtype Digit is Integer range 0 .. 15;

   type Type_Set is (Lower_Case, Upper_Case); -- same as Text_IO.Type_Set
   pragma Discard_Names (Type_Set);

   function Digits_Width (
      Value : Long_Long_Integer_Types.Word_Unsigned;
      Base : Number_Base := 10)
      return Positive;
   function Digits_Width (
      Value : Long_Long_Integer_Types.Long_Long_Unsigned;
      Base : Number_Base := 10)
      return Positive;

   procedure Image (
      Value : Digit;
      Item : out Character;
      Set : Type_Set := Upper_Case);

   procedure Image (
      Value : Long_Long_Integer_Types.Word_Unsigned;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Set : Type_Set := Upper_Case;
      Width : Positive := 1;
      Fill : Character := '0';
      Error : out Boolean);

   procedure Image (
      Value : Long_Long_Integer_Types.Long_Long_Unsigned;
      Item : out String;
      Last : out Natural;
      Base : Number_Base := 10;
      Set : Type_Set := Upper_Case;
      Width : Positive := 1;
      Fill : Character := '0';
      Error : out Boolean);

   procedure Value (
      Item : Character;
      Result : out Digit;
      Error : out Boolean);

   procedure Value (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Integer_Types.Word_Unsigned;
      Base : Number_Base := 10;
      Skip_Underscore : Boolean := False;
      Error : out Boolean);

   procedure Value (
      Item : String;
      Last : out Natural;
      Result : out Long_Long_Integer_Types.Long_Long_Unsigned;
      Base : Number_Base := 10;
      Skip_Underscore : Boolean := False;
      Error : out Boolean);

   --  sign marks, compatible with Ada.Formatting

   type Sign_Marks is record
      Minus, Zero, Plus : Character;
   end record;
   for Sign_Marks'Size use Character'Size * 4;
   pragma Suppress_Initialization (Sign_Marks);

   No_Sign : constant Character := Character'Val (16#ff#);

   --  Note: Literals of array of Character make undesirable static strings.
   --  Literals of word-size record can be expected to be immediate values.

   --  utility

   procedure Fill_Padding (Item : out String; Pad : Character);

end System.Formatting;
