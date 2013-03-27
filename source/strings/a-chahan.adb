with Ada.Characters.Inside.Maps.Case_Folding;
with Ada.Characters.Inside.Maps.Case_Mapping;
with Ada.Characters.Inside.Sets.General_Category;
with Ada.Characters.Inside.Sets.Constants;
with System.UTF_Conversions;
package body Ada.Characters.Handling is

   function Is_Control (Item : Character) return Boolean is
   begin
      return Inside.Sets.Is_In (
         Item,
         Inside.Sets.General_Category.Control);
   end Is_Control;

   function Is_Graphic (Item : Character) return Boolean is
   begin
      return Inside.Sets.Is_In (
         Item,
         Inside.Sets.Constants.Graphic_Set);
   end Is_Graphic;

   function Is_Letter (Item : Character) return Boolean is
   begin
      return Inside.Sets.Is_In (
         Item,
         Inside.Sets.Constants.Letter_Set);
   end Is_Letter;

   function Is_Lower (Item : Character) return Boolean is
   begin
      return Inside.Sets.Is_In (
         Item,
         Inside.Sets.General_Category.Lowercase_Letter);
   end Is_Lower;

   function Is_Upper (Item : Character) return Boolean is
   begin
      return Inside.Sets.Is_In (
         Item,
         Inside.Sets.General_Category.Uppercase_Letter);
   end Is_Upper;

   function Is_Digit (Item : Character) return Boolean is
   begin
      return Inside.Sets.Is_In (
         Item,
         Inside.Sets.Constants.Decimal_Digit_Set);
   end Is_Digit;

   function Is_Hexadecimal_Digit (Item : Character) return Boolean is
   begin
      return Inside.Sets.Is_In (
         Item,
         Inside.Sets.Constants.Hexadecimal_Digit_Set);
   end Is_Hexadecimal_Digit;

   function Is_Alphanumeric (Item : Character) return Boolean is
   begin
      return Inside.Sets.Is_In (
         Item,
         Inside.Sets.Constants.Alphanumeric_Set);
   end Is_Alphanumeric;

   function Is_Special (Item : Character) return Boolean is
   begin
      return Inside.Sets.Is_In (
         Item,
         Inside.Sets.Constants.Special_Set);
   end Is_Special;

   function To_Lower (Item : Character) return Character is
   begin
      return Inside.Maps.Value (
         Inside.Maps.Case_Mapping.Lower_Case_Map,
         Item);
   end To_Lower;

   function To_Upper (Item : Character) return Character is
   begin
      return Inside.Maps.Value (
         Inside.Maps.Case_Mapping.Upper_Case_Map,
         Item);
   end To_Upper;

   function To_Case_Folding (Item : Character) return Character is
   begin
      return Inside.Maps.Value (
         Inside.Maps.Case_Folding.Case_Folding_Map,
         Item);
   end To_Case_Folding;

   function To_Lower (Item : String) return String is
      Result : String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
   begin
      Inside.Maps.Translate (
         Item,
         Inside.Maps.Case_Mapping.Lower_Case_Map,
         Result,
         Last);
      return Result (1 .. Last);
   end To_Lower;

   function To_Upper (Item : String) return String is
      Result : String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
   begin
      Inside.Maps.Translate (
         Item,
         Inside.Maps.Case_Mapping.Upper_Case_Map,
         Result,
         Last);
      return Result (1 .. Last);
   end To_Upper;

   function To_Case_Folding (Item : String) return String is
      Result : String (
         1 ..
         Item'Length * System.UTF_Conversions.UTF_8_Max_Length);
      Last : Natural;
   begin
      Inside.Maps.Translate (
         Item,
         Inside.Maps.Case_Folding.Case_Folding_Map,
         Result,
         Last);
      return Result (1 .. Last);
   end To_Case_Folding;

   function Is_ISO_646 (Item : Character) return Boolean is
   begin
      return Item in ISO_646;
   end Is_ISO_646;

   function Is_ISO_646 (Item : String) return Boolean is
   begin
      for I in Item'Range loop
         if Item (I) not in ISO_646 then
            return False;
         end if;
      end loop;
      return True;
   end Is_ISO_646;

   function To_ISO_646 (Item : Character; Substitute : ISO_646 := ' ')
      return ISO_646 is
   begin
      if Is_ISO_646 (Item) then
         return Item;
      else
         return Substitute;
      end if;
   end To_ISO_646;

   function To_ISO_646 (Item : String; Substitute : ISO_646 := ' ')
      return String is
   begin
      return Result : String (1 .. Item'Length) do
         for I in Result'Range loop
            Result (I) := To_ISO_646 (
               Item (Item'First - Result'First + I),
               Substitute);
         end loop;
      end return;
   end To_ISO_646;

end Ada.Characters.Handling;
