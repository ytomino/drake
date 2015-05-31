pragma License (Unrestricted);
with Ada.Text_IO.Editing;
package Ada.Wide_Text_IO.Editing is

   --  modified
--  type Picture is private;
   subtype Picture is Text_IO.Editing.Picture;

   function Valid (
      Pic_String : String;
      Blank_When_Zero : Boolean := False)
      return Boolean
      renames Text_IO.Editing.Valid;

   function To_Picture (
      Pic_String : String;
      Blank_When_Zero : Boolean := False)
      return Picture
      renames Text_IO.Editing.To_Picture;

   function Pic_String (Pic : Picture) return String
      renames Text_IO.Editing.Pic_String;
   function Blank_When_Zero (Pic : Picture) return Boolean
      renames Text_IO.Editing.Blank_When_Zero;

   Max_Picture_Length : constant := 30; -- implementation_defined

   Picture_Error : exception
      renames Text_IO.Editing.Picture_Error;

   Default_Currency : constant Wide_String := "$";
   Default_Fill : constant Wide_Character := '*';
   Default_Separator : constant Wide_Character := ',';
   Default_Radix_Mark : constant Wide_Character := '.';

   generic
      type Num is delta <> digits <>;
      Default_Currency : Wide_String := Editing.Default_Currency;
      Default_Fill : Wide_Character := Editing.Default_Fill;
      Default_Separator : Wide_Character := Editing.Default_Separator;
      Default_Radix_Mark : Wide_Character := Editing.Default_Radix_Mark;
   package Decimal_Output is

      --  for renaming
      package Strings is new Text_IO.Editing.Decimal_Output (Num);

      function Length (
         Pic : Picture;
         Currency : Wide_String := Default_Currency)
         return Natural
         renames Strings.Overloaded_Length;

      function Valid (
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency)
         return Boolean
         renames Strings.Overloaded_Valid;

      function Image (
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency;
         Fill : Wide_Character := Default_Fill;
         Separator : Wide_Character := Default_Separator;
         Radix_Mark : Wide_Character := Default_Radix_Mark)
         return Wide_String
         renames Strings.Overloaded_Image;

      procedure Put (
         File : File_Type; -- Output_File_Type
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency;
         Fill : Wide_Character := Default_Fill;
         Separator : Wide_Character := Default_Separator;
         Radix_Mark : Wide_Character := Default_Radix_Mark)
         renames Strings.Overloaded_Put;

      procedure Put (
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency;
         Fill : Wide_Character := Default_Fill;
         Separator : Wide_Character := Default_Separator;
         Radix_Mark : Wide_Character := Default_Radix_Mark)
         renames Strings.Overloaded_Put;

      procedure Put (
         To : out Wide_String;
         Item : Num;
         Pic : Picture;
         Currency : Wide_String := Default_Currency;
         Fill : Wide_Character := Default_Fill;
         Separator : Wide_Character := Default_Separator;
         Radix_Mark : Wide_Character := Default_Radix_Mark)
         renames Strings.Overloaded_Put;

   end Decimal_Output;

end Ada.Wide_Text_IO.Editing;
