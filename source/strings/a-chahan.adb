with Ada.Strings.Naked_Maps.Case_Folding;
with Ada.Strings.Naked_Maps.Case_Mapping;
with Ada.Strings.Naked_Maps.General_Category;
with Ada.Strings.Naked_Maps.Set_Constants;
package body Ada.Characters.Handling is

   function Is_Control (Item : Character) return Boolean is
   begin
      return Strings.Naked_Maps.Is_In (
         Item,
         Strings.Naked_Maps.General_Category.Control.all);
   end Is_Control;

   function Is_Graphic (Item : Character) return Boolean is
   begin
      return Strings.Naked_Maps.Is_In (
         Item,
         Strings.Naked_Maps.Set_Constants.Graphic_Set.all);
   end Is_Graphic;

   function Is_Letter (Item : Character) return Boolean is
   begin
      return Strings.Naked_Maps.Is_In (
         Item,
         Strings.Naked_Maps.Set_Constants.Letter_Set.all);
   end Is_Letter;

   function Is_Lower (Item : Character) return Boolean is
   begin
      return Strings.Naked_Maps.Is_In (
         Item,
         Strings.Naked_Maps.General_Category.Lowercase_Letter.all);
   end Is_Lower;

   function Is_Upper (Item : Character) return Boolean is
   begin
      return Strings.Naked_Maps.Is_In (
         Item,
         Strings.Naked_Maps.General_Category.Uppercase_Letter.all);
   end Is_Upper;

   function Is_Digit (Item : Character) return Boolean is
   begin
      return Strings.Naked_Maps.Is_In (
         Item,
         Strings.Naked_Maps.Set_Constants.Decimal_Digit_Set.all);
   end Is_Digit;

   function Is_Hexadecimal_Digit (Item : Character) return Boolean is
   begin
      return Strings.Naked_Maps.Is_In (
         Item,
         Strings.Naked_Maps.Set_Constants.Hexadecimal_Digit_Set.all);
   end Is_Hexadecimal_Digit;

   function Is_Alphanumeric (Item : Character) return Boolean is
   begin
      return Strings.Naked_Maps.Is_In (
         Item,
         Strings.Naked_Maps.Set_Constants.Alphanumeric_Set.all);
   end Is_Alphanumeric;

   function Is_Special (Item : Character) return Boolean is
   begin
      return Strings.Naked_Maps.Is_In (
         Item,
         Strings.Naked_Maps.Set_Constants.Special_Set.all);
   end Is_Special;

   function To_Lower (Item : Character) return Character is
   begin
      return Strings.Naked_Maps.Value (
         Strings.Naked_Maps.Case_Mapping.Lower_Case_Map.all,
         Item);
   end To_Lower;

   function To_Upper (Item : Character) return Character is
   begin
      return Strings.Naked_Maps.Value (
         Strings.Naked_Maps.Case_Mapping.Upper_Case_Map.all,
         Item);
   end To_Upper;

   function To_Case_Folding (Item : Character) return Character is
   begin
      return Strings.Naked_Maps.Value (
         Strings.Naked_Maps.Case_Folding.Case_Folding_Map.all,
         Item);
   end To_Case_Folding;

   function To_Lower (Item : String) return String is
      Result : String (
         1 ..
         Item'Length * Conversions.Max_Length_In_String);
      Last : Natural;
   begin
      Strings.Naked_Maps.Translate (
         Item,
         Strings.Naked_Maps.Case_Mapping.Lower_Case_Map.all,
         Result,
         Last);
      return Result (1 .. Last);
   end To_Lower;

   function To_Upper (Item : String) return String is
      Result : String (
         1 ..
         Item'Length * Conversions.Max_Length_In_String);
      Last : Natural;
   begin
      Strings.Naked_Maps.Translate (
         Item,
         Strings.Naked_Maps.Case_Mapping.Upper_Case_Map.all,
         Result,
         Last);
      return Result (1 .. Last);
   end To_Upper;

   function To_Case_Folding (Item : String) return String is
      Result : String (
         1 ..
         Item'Length * Conversions.Max_Length_In_String);
      Last : Natural;
   begin
      Strings.Naked_Maps.Translate (
         Item,
         Strings.Naked_Maps.Case_Folding.Case_Folding_Map.all,
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
      return String
   is
      Length : constant Natural := Item'Length;
   begin
      return Result : String (1 .. Length) do
         for I in 0 .. Length - 1 loop
            Result (Result'First + I) :=
               To_ISO_646 (Item (Item'First + I), Substitute);
         end loop;
      end return;
   end To_ISO_646;

end Ada.Characters.Handling;
