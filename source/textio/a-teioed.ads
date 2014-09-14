pragma License (Unrestricted);
package Ada.Text_IO.Editing is

   type Picture is private;

   function Valid (
      Pic_String : String;
      Blank_When_Zero : Boolean := False)
      return Boolean;

   function To_Picture (
      Pic_String : String;
      Blank_When_Zero : Boolean := False)
      return Picture;

   function Pic_String (Pic : Picture) return String;
   function Blank_When_Zero (Pic : Picture) return Boolean;
   pragma Inline (Blank_When_Zero);

   Max_Picture_Length : constant := 30; -- implementation_defined

   Picture_Error : exception;

   Default_Currency : constant String := "$";
   Default_Fill : constant Character := '*';
   Default_Separator : constant Character := ',';
   Default_Radix_Mark : constant Character := '.';

   generic
      type Num is delta <> digits <>;
      Default_Currency : String := Editing.Default_Currency;
      Default_Fill : Character := Editing.Default_Fill;
      Default_Separator : Character := Editing.Default_Separator;
      Default_Radix_Mark : Character := Editing.Default_Radix_Mark;
   package Decimal_Output is

      --  extended
      function Overloaded_Length (Pic : Picture; Currency : String)
         return Natural;
      function Overloaded_Length (Pic : Picture; Currency : Wide_String)
         return Natural;
      function Overloaded_Length (Pic : Picture; Currency : Wide_Wide_String)
         return Natural;

      function Length (Pic : Picture; Currency : String := Default_Currency)
         return Natural
         renames Overloaded_Length;

      --  extended
      function Overloaded_Valid (
         Item : Num;
         Pic : Picture;
         Currency : String)
         return Boolean;
      function Overloaded_Valid (
         Item : Num;
         Pic : Picture;
         Currency : Wide_String)
         return Boolean;
      function Overloaded_Valid (
         Item : Num;
         Pic : Picture;
         Currency : Wide_Wide_String)
         return Boolean;

      function Valid (
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency)
         return Boolean
         renames Overloaded_Valid;

      --  extended
      function Overloaded_Image (
         Item : Num;
         Pic : Picture;
         Currency : String;
         Fill : Character;
         Separator : Character;
         Radix_Mark : Character)
         return String;
      function Overloaded_Image (
         Item : Num;
         Pic : Picture;
         Currency : Wide_String;
         Fill : Wide_Character;
         Separator : Wide_Character;
         Radix_Mark : Wide_Character)
         return Wide_String;
      function Overloaded_Image (
         Item : Num;
         Pic : Picture;
         Currency : Wide_Wide_String;
         Fill : Wide_Wide_Character;
         Separator : Wide_Wide_Character;
         Radix_Mark : Wide_Wide_Character)
         return Wide_Wide_String;

      function Image (
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency;
         Fill : Character := Default_Fill;
         Separator : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark)
         return String
         renames Overloaded_Image;

      --  extended
      procedure Overloaded_Put (
         File : File_Type;
         Item : Num;
         Pic : Picture;
         Currency : String;
         Fill : Character;
         Separator : Character;
         Radix_Mark : Character);
      procedure Overloaded_Put (
         File : File_Type;
         Item : Num;
         Pic : Picture;
         Currency : Wide_String;
         Fill : Wide_Character;
         Separator : Wide_Character;
         Radix_Mark : Wide_Character);
      procedure Overloaded_Put (
         File : File_Type;
         Item : Num;
         Pic : Picture;
         Currency : Wide_Wide_String;
         Fill : Wide_Wide_Character;
         Separator : Wide_Wide_Character;
         Radix_Mark : Wide_Wide_Character);

      procedure Put (
         File : File_Type;
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency;
         Fill : Character := Default_Fill;
         Separator : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark)
         renames Overloaded_Put;

      --  extended
      procedure Overloaded_Put (
         Item : Num;
         Pic : Picture;
         Currency : String;
         Fill : Character;
         Separator : Character;
         Radix_Mark : Character);
      procedure Overloaded_Put (
         Item : Num;
         Pic : Picture;
         Currency : Wide_String;
         Fill : Wide_Character;
         Separator : Wide_Character;
         Radix_Mark : Wide_Character);
      procedure Overloaded_Put (
         Item : Num;
         Pic : Picture;
         Currency : Wide_Wide_String;
         Fill : Wide_Wide_Character;
         Separator : Wide_Wide_Character;
         Radix_Mark : Wide_Wide_Character);

      procedure Put (
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency;
         Fill : Character := Default_Fill;
         Separator : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark)
         renames Overloaded_Put;

      --  extended
      procedure Overloaded_Put (
         To : out String;
         Item : Num;
         Pic : Picture;
         Currency : String;
         Fill : Character;
         Separator : Character;
         Radix_Mark : Character);
      procedure Overloaded_Put (
         To : out Wide_String;
         Item : Num;
         Pic : Picture;
         Currency : Wide_String;
         Fill : Wide_Character;
         Separator : Wide_Character;
         Radix_Mark : Wide_Character);
      procedure Overloaded_Put (
         To : out Wide_Wide_String;
         Item : Num;
         Pic : Picture;
         Currency : Wide_Wide_String;
         Fill : Wide_Wide_Character;
         Separator : Wide_Wide_Character;
         Radix_Mark : Wide_Wide_Character);

      procedure Put (
         To : out String;
         Item : Num;
         Pic : Picture;
         Currency : String := Default_Currency;
         Fill : Character := Default_Fill;
         Separator : Character := Default_Separator;
         Radix_Mark : Character := Default_Radix_Mark)
         renames Overloaded_Put;

   end Decimal_Output;

private

   type Dollar_Position is (None, Previous);
   pragma Discard_Names (Dollar_Position);

   type Picture is record
      Expanded : String (1 .. Max_Picture_Length);
      Length : Natural;
      Has_V : Boolean; -- zero width
      Has_Dollar : Dollar_Position; -- replaced to Currency
      Blank_When_Zero : Boolean;
      Real_Blank_When_Zero : Boolean;
      First_Sign_Position : Natural;
      Radix_Position : Positive;
      Aft : Natural;
   end record;

end Ada.Text_IO.Editing;
