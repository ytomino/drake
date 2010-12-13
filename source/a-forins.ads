pragma License (Unrestricted);
--  implementation package
with System.Formatting;
package Ada.Formatting.Inside is
   pragma Pure;

   Integer_Width : constant := Long_Long_Integer'Width + 4; -- (16##)

   procedure Integer_Image (
      To : out String;
      Last : out Natural;
      Item : Integer;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0');

   procedure Integer_Image (
      To : out String;
      Last : out Natural;
      Item : Long_Long_Integer;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0');

   Modular_Width : constant :=
      System.Formatting.Longest_Unsigned'Width + 4; -- (16##)

   procedure Modular_Image (
      To : out String;
      Last : out Natural;
      Item : System.Formatting.Unsigned;
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0');

   procedure Modular_Image (
      To : out String;
      Last : out Natural;
      Item : System.Formatting.Longest_Unsigned;
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0');

end Ada.Formatting.Inside;
