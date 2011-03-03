with Ada.Characters.Inside.Sets.General_Category;
package body Ada.Characters.Inside.Sets.Constants is
   pragma Suppress (All_Checks);
   use type Interfaces.Integer_32;

   Decimal_Digit_Set_Data : aliased Characters.Inside.Sets.Character_Set := (
      Length => 1,
      Reference_Count => -1,
      Items => (1 => ('0', '9')));

   function Decimal_Digit_Set return not null access Sets.Character_Set is
   begin
      return Decimal_Digit_Set_Data'Access;
   end Decimal_Digit_Set;

   Hexadecimal_Digit_Set_Data :
      aliased Characters.Inside.Sets.Character_Set := (
      Length => 3,
      Reference_Count => -1,
      Items => (('0', '9'), ('A', 'F'), ('a', 'f')));

   function Hexadecimal_Digit_Set return not null access Sets.Character_Set is
   begin
      return Hexadecimal_Digit_Set_Data'Access;
   end Hexadecimal_Digit_Set;

   ISO_646_Set_Data : aliased Characters.Inside.Sets.Character_Set := (
      Length => 1,
      Reference_Count => -1,
      Items => (1 => (Character_Type'Val (0), Character_Type'Val (16#7F#))));

   function ISO_646_Set return not null access Sets.Character_Set is
   begin
      return ISO_646_Set_Data'Access;
   end ISO_646_Set;

   Wide_Character_Set_Data : aliased Characters.Inside.Sets.Character_Set := (
      Length => 2,
      Reference_Count => -1,
      Items => (
         (Character_Type'Val (0), Character_Type'Val (16#D7FF#)),
         (Character_Type'Val (16#E000#), Character_Type'Val (16#FFFF#))));

   function Wide_Character_Set return not null access Sets.Character_Set is
   begin
      return Wide_Character_Set_Data'Access;
   end Wide_Character_Set;

   type Character_Set_Access is access Sets.Character_Set;

   function Total_Length (Source : Character_Ranges_Array) return Natural;
   --  local
   function Total_Length (Source : Character_Ranges_Array) return Natural is
      Result : Natural := 0;
   begin
      for I in Source'Range loop
         Result := Result + Source (I)'Length;
      end loop;
      return Result;
   end Total_Length;

   Letter_Set_Data : Character_Set_Access := null;

   function Letter_Set return not null access Sets.Character_Set is
   begin
      if Letter_Set_Data = null then
         declare
            Source : constant Character_Ranges_Array := (
               General_Category.Lowercase_Letter.Items'Unrestricted_Access,
               General_Category.Uppercase_Letter.Items'Unrestricted_Access,
               General_Category.Titlecase_Letter.Items'Unrestricted_Access,
               General_Category.Modifier_Letter.Items'Unrestricted_Access,
               General_Category.Other_Letter.Items'Unrestricted_Access);
            Items : Character_Ranges (1 .. Total_Length (Source));
            Last : Natural;
         begin
            Merge (Items, Last, Source);
            Letter_Set_Data := new Sets.Character_Set'(
               Length => Last,
               Reference_Count => -1,
               Items => Items (1 .. Last));
         end;
      end if;
      return Letter_Set_Data;
   end Letter_Set;

   Alphanumeric_Set_Data : Character_Set_Access := null;

   function Alphanumeric_Set return not null access Sets.Character_Set is
   begin
      if Alphanumeric_Set_Data = null then
         declare
            Source : constant Character_Ranges_Array := (
               Letter_Set.Items'Unrestricted_Access,
               General_Category.Decimal_Number.Items'Unrestricted_Access,
               General_Category.Letter_Number.Items'Unrestricted_Access,
               General_Category.Other_Number.Items'Unrestricted_Access);
            Items : Character_Ranges (1 .. Total_Length (Source));
            Last : Natural;
         begin
            Merge (Items, Last, Source);
            Alphanumeric_Set_Data := new Sets.Character_Set'(
               Length => Last,
               Reference_Count => -1,
               Items => Items (1 .. Last));
         end;
      end if;
      return Alphanumeric_Set_Data;
   end Alphanumeric_Set;

   Special_Set_Data : Character_Set_Access := null;

   function Special_Set return not null access Sets.Character_Set is
   begin
      if Special_Set_Data = null then
         declare
            Source : constant Character_Ranges_Array := (
               General_Category.Nonspacing_Mark.Items'Unrestricted_Access,
               General_Category.Enclosing_Mark.Items'Unrestricted_Access,
               General_Category.Spacing_Mark.Items'Unrestricted_Access,
               General_Category.Space_Separator.Items'Unrestricted_Access,
               General_Category.Dash_Punctuation.Items'Unrestricted_Access,
               General_Category.Open_Punctuation.Items'Unrestricted_Access,
               General_Category.Close_Punctuation.Items'Unrestricted_Access,
               General_Category.Connector_Punctuation.
                  Items'Unrestricted_Access,
               General_Category.Other_Punctuation.Items'Unrestricted_Access,
               General_Category.Math_Symbol.Items'Unrestricted_Access,
               General_Category.Currency_Symbol.Items'Unrestricted_Access,
               General_Category.Modifier_Symbol.Items'Unrestricted_Access,
               General_Category.Other_Symbol.Items'Unrestricted_Access,
               General_Category.Initial_Punctuation.Items'Unrestricted_Access,
               General_Category.Final_Punctuation.Items'Unrestricted_Access);
            Items : Character_Ranges (1 .. Total_Length (Source));
            Last : Natural;
         begin
            Merge (Items, Last, Source);
            Special_Set_Data := new Sets.Character_Set'(
               Length => Last,
               Reference_Count => -1,
               Items => Items (1 .. Last));
         end;
      end if;
      return Special_Set_Data;
   end Special_Set;

   Graphic_Set_Data : Character_Set_Access := null;

   function Graphic_Set return not null access Sets.Character_Set is
   begin
      if Graphic_Set_Data = null then
         declare
            Source : constant Character_Ranges_Array := (
               Alphanumeric_Set.Items'Unrestricted_Access,
               Special_Set.Items'Unrestricted_Access);
            Items : Character_Ranges (1 .. Total_Length (Source));
            Last : Natural;
         begin
            Merge (Items, Last, Source);
            Graphic_Set_Data := new Sets.Character_Set'(
               Length => Last,
               Reference_Count => -1,
               Items => Items (1 .. Last));
         end;
      end if;
      return Graphic_Set_Data;
   end Graphic_Set;

end Ada.Characters.Inside.Sets.Constants;
