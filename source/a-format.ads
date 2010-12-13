pragma License (Unrestricted);
--  extended package
package Ada.Formatting is
   pragma Pure;

   type Form_Type is (Simple, Ada);

   subtype Number_Base is Integer range 2 .. 16; -- same as Text_IO.Number_Base

   None : constant Character := Character'Val (0);

   type Casing_Type is (Upper, Lower);
   --  same as System.Formating.Casing_Type

   generic
      type T is range <>;
      Form : Form_Type := Ada;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0';
   function Integer_Image (Item : T) return String;

   generic
      type T is mod <>;
      Form : Form_Type := Ada;
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Width : Positive := 1;
      Padding : Character := '0';
   function Modular_Image (Item : T) return String;

   generic
      type T is digits <>;
      Form : Form_Type := Ada;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Fore_Width : Positive := 1;
      Fore_Padding : Character := '0';
      Aft_Width : Positive := T'Digits - 1;
      Exponent_Mark : Character := 'E';
      Exponent_Minus_Sign : Character := '-';
      Exponent_Zero_Sign : Character := '+';
      Exponent_Plus_Sign : Character := '+';
      Exponent_Width : Positive := 2;
      Exponent_Padding : Character := '0';
      NaN : String := "NAN";
      Infinity : String := "INF";
   function Float_Image (Item : T) return String;

   generic
      type T is delta <>;
      Form : Form_Type := Ada;
      Exponent : Boolean := False;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Base : Number_Base := 10;
      Casing : Casing_Type := Upper;
      Fore_Width : Positive := 1;
      Fore_Padding : Character := '0';
      Aft_Width : Positive := T'Aft;
      Exponent_Mark : Character := 'E';
      Exponent_Minus_Sign : Character := '-';
      Exponent_Zero_Sign : Character := '+';
      Exponent_Plus_Sign : Character := '+';
      Exponent_Width : Positive := 2;
      Exponent_Padding : Character := '0';
   function Fixed_Image (Item : T) return String;

   generic
      type T is delta <> digits <>;
      Form : Form_Type := Ada;
      pragma Unreferenced (Form); -- 10-based only
      Exponent : Boolean := False;
      Minus_Sign : Character := '-';
      Zero_Sign : Character := ' ';
      Plus_Sign : Character := ' ';
      Fore_Width : Positive := 1;
      Fore_Padding : Character := '0';
      Aft_Width : Positive := T'Aft;
      Exponent_Mark : Character := 'E';
      Exponent_Minus_Sign : Character := '-';
      Exponent_Zero_Sign : Character := '+';
      Exponent_Plus_Sign : Character := '+';
      Exponent_Width : Positive := 2;
      Exponent_Padding : Character := '0';
   function Decimal_Image (Item : T) return String;

end Ada.Formatting;
